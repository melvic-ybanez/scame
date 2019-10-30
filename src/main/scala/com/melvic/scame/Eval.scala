package com.melvic.scame

import com.melvic.scame.Env.EnvConfig
import com.melvic.scame.ErrorCode.{ExprMismatch, IncorrectParamCount, TooFewArguments}
import com.melvic.scame.Eval.foldS
import com.melvic.scame.SExpr._
import zio.ZIO

object Eval {
  type EvaluationE[E] = ZIO[EvalConfig, ErrorCode, E]
  type Evaluation = EvaluationE[SExpr]
  type PartialEval = PartialFunction[SExpr, Evaluation]

  final case class EvalConfig(expr: SExpr, env: Env)

  def apply(expr: SExpr, env: Env): Evaluation = apply.provide(EvalConfig(expr, env))

  def apply(expression: SExpr): Evaluation =
    apply.provideSome[EvalConfig](_.copy(expr = expression))

  def apply: Evaluation = ZIO.accessM { case EvalConfig(expr, _) =>
    val eval = atom orElse symbol orElse emptyList orElse
      pair orElse define orElse sLambda orElse
      cond orElse let orElse arithmetic orElse
      quote
    eval(expr)
  }

  def atom: PartialEval = { case atom: Atom => atom.! }

  def symbol: PartialEval = { case SSymbol(name) =>
    provideNameToEnv(name, Env.globalSearch)
  }

  def emptyList: PartialEval = { case SNil => SNil.! }

  def pair: PartialEval = {
    case Cons(Cons, Cons(_, SNil)) => IncorrectParamCount(2, 1).!
    case Cons(Cons, Cons(head, tail)) => for {
      h <- Eval(head)
      t <- Eval(tail)
    } yield t match {
      // If the tail is a list, the whole cons is a proper list.
      case tList: SList => Cons(h, tList)
      // Otherwise, it remains an improper list.
      case _ => Pair(h, t)
    }
    case Cons(Cons, tail) =>
      ExprMismatch("pair" +: Vector(), tail).!
  }

  def define: PartialEval = {
    case Cons(Define, Cons(SSymbol(name), SNil)) => Eval(Define(name, SNil))
    case Cons(Define, Cons(SSymbol(name), Cons(value, SNil))) => Eval(value).flatMap { v =>
      Eval(Define(name, v))
    }
    case Cons(Define, body) => ExprMismatch(
      Constants.Symbol +: Constants.Pair +: Vector(), body).!
    case Define(name, value) => register(name, value).map(Definition)
  }

  def sLambda: PartialEval = {
    // Construct a lambda object. Both the params and the body shouldn't
    // be evaluated.
    case Cons(Lambda, Cons(params, body)) => Lambda(params, body).!

    case Cons(Lambda(params: SList, body), args: SList) =>
      def recurse(env: Env): (SList, SList) => EvaluationE[Env] = {
        case (SNil, _) | (_, SNil) => ZIO.succeed(env)
        case (Cons(SSymbol(param), t), Cons(arg, t1)) => for {
          evaluatedArg <- Eval(arg)
          newEnv <- register(param, evaluatedArg)
          result <- recurse(newEnv)(t, t1)
        } yield result
        case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h).!
      }

      for {
        config <- ZIO.environment[EvalConfig]
        env <- recurse(config.env)(params, args)
        result <- Eval(body, env)
      } yield result

    // If the parameter is a symbol instead of a list, set it to the
    // whole argument list.
    case Cons(Lambda(SSymbol(param), body), args) =>
      for {
        argList <- args.asScalaList.foldLeft[EvaluationE[SList]](SNil.!) { (acc, arg) =>
          for {
            tail <- acc
            head <- Eval(arg)
          } yield Cons(head, tail)
        }
        env <- register(param, argList)
        result <- Eval(body, env)
      } yield result
  }

  def quote: PartialEval = {
    case Cons(Quote, Cons(arg, _)) => arg.!
  }

  def cond: PartialEval = {
    case Cons(Cond, Cons(Cons(pred, Cons(body, _)), _)) if !SExpr.falsy(pred) => Eval(body)
    case Cons(Cond, Cons(Cons(pred, _), SNil)) if SExpr.falsy(pred) => SNil.!
    case Cons(Cond, Cons(Cons(pred, _), rest)) if SExpr.falsy(pred) => Eval(Cons(Cond, rest))
    case Cons(Cond, Cons(Cons(sexpr, body), rest)) => for {
      head <- Eval(sexpr)
      result <- Eval(Cons(Cond, Cons(Cons(head, body), rest)))
    } yield result
    case Cons(Cond, expr) => ExprMismatch(Vector(s"List of ${Constants.Pair}s"), expr).!
  }

  /**
   * A let expression ideally takes two arguments: the list of symbol-value pairs
   * and the main body of computation.
   */
  def let: PartialEval = {
    // If the head of the first argument is a non-empty list, see if you can make
    // a definition out of it by delegating to the `define` special form.
    case Cons(Let, Cons(Cons(define: Cons, defineRest), body)) => for {
      definedHead <- Eval(Cons(Define, define))
      result <- definedHead match {
        case Definition(env) => Eval(Cons(Let, Cons(defineRest, body)), env)
      }
    } yield result

    // If there aren't any definitions to evaluate, evaluate the body.
    case Cons(Let, Cons(SNil, Cons(body, SNil))) => Eval(body)

    case Cons(Let, expr) => ExprMismatch(Vector("A list of pairs for bindings"), expr).!
  }

  def arithmetic: PartialEval = {
    def add: PartialEval = {
      case Cons(Add, args) => foldS(args, SInt(0)) {
        case (SInt(a), SInt(b)) => SInt(a + b).!
        case (SInt(a), SRational(n, d)) => SRational(a * d + n, d).!
        case (SInt(a), SReal(b)) => SReal(a + b).!
        case (r: SRational, i: SInt) => binOp(Add, i, r) // reverse
        case (SRational(n1, d1), SRational(n2, d2)) =>
          val lcd = SMath.lcm(d1, d2)
          SRational(lcd / d1 * n1 + lcd / d2 * n2, lcd).!
        case (rat: SRational, real: SReal) =>
          // evaluate the two as real numbers
          binOp(Add, real, SMath.rationalToReal(rat))
        case (r: SReal, i: SInt) => binOp(Add, i, r) // reverse
        case (r: SReal, f: SRational) => binOp(Add, f, r) // reverse
        case (SReal(a), SReal(b)) => SReal(a + b).!

        case (_, expr) => nonNumber(expr)
      }
    }

    /**
     * Computes the difference. If there are more than one numbers,
     * negate all of them but the first one, and compute their sum.
     */
    def subtract: PartialEval = reverseOp(Subtract, Add)(negate)

    def multiply: PartialEval = {
      case Cons(Multiply, args: SList) => foldS(args, SInt(1)) {
        case (SInt(a), SInt(b)) => SInt(a * b).!
        case (SInt(a), SRational(n, d)) => SRational(a * n, d).!
        case (SInt(a), SReal(b)) => SReal(a * b).!
        case (r: SRational, i: SInt) => binOp(Multiply, i, r)
        case (SRational(n, d), SRational(n1, d1)) =>
          // multiply the numerators and denominators
          val num = n * n1
          val denom = d * d1

          // simplify the resulting fraction
          val gcd = BigInt(num).gcd(BigInt(denom)).intValue
          SRational(num / gcd, denom / gcd).!
        case (rat: SRational, real: SReal) =>
          binOp(Multiply, real, SMath.rationalToReal(rat))
        case (r: SReal, i: SInt) => binOp(Multiply, i, r)
        case (r: SReal, f: SRational) => binOp(Multiply, f, r)
        case (SReal(a), SReal(b)) => SReal(a * b).!

        case (_, expr) => nonNumber(expr)
      }
    }

    /**
     * Computes the difference. If there are more than one numbers,
     * get the reciprocal of each item but the first one, and compute their product.
     * TODO: Division-by-zero stuff.
     */
    def divide: PartialEval = reverseOp(Divide, Multiply) {
      case SInt(a) => SRational(1, a)
      case SRational(n, d) => SRational(d, n)
      case r: SReal =>
        val SRational(w, f) = SMath.realToRational(r)
        SRational(f, w)
    }

    def reverseOp(op: Arithmetic, op1: Arithmetic)(f: SNumber => SNumber): PartialEval = {
      case Cons(`op`, SNil) => TooFewArguments(1, 0).!
      case Cons(`op`, Cons(n: SNumber, SNil)) => f(n).!
      case Cons(`op`, Cons(h: SNumber, t)) => for {
        reverse <- foldS(t, SNil) {
          case (acc: SList, n: SNumber) => Cons(f(n), acc).!
          case (_, n) => nonNumber(n)
        }

        // Only accepts lists. We have to manually pattern match
        // because foldS is weakly typed.
        reversedNumbers <- reverse match {
          case l: SList => l.!
          case e => nonNumber(e)
        }

        diff <- Eval(Cons(op1, Cons(h, reversedNumbers)))
      } yield diff
      case Cons(`op`, Cons(expr, _)) => nonNumber(expr)
    }

    def negate: SNumber => SNumber = {
      case SInt(a) => SInt(-a)
      case SRational(n, d) => SRational(-n, d)
      case SReal(n) => SReal(-n)
    }

    def nonNumber(expr: SExpr) = ExprMismatch(Vector(Constants.Number), expr).!

    // TODO: Simplify resulting rationals to avoid awkward values
    //  such as 1/1.
    add orElse subtract orElse multiply orElse divide
  }

  def provideNameToEnv[A](name: String, env: ZIO[EnvConfig, ErrorCode, A]) =
    env.provideSome[EvalConfig](e => (name, e.env))

  def register(name: String, expr: SExpr) = provideNameToEnv(name, Env.register(expr))

  /**
   * Short-circuiting fold designed specifically for evaluated s-expressions.
   * TODO: This is not tail-recursive
   */
  def foldS(sList: SList, acc: SExpr)(f: (SExpr, SExpr) => Evaluation): Evaluation = sList match {
    case SNil => acc.!
    case Cons(head, tail) => for {
      a <- f(acc, head)
      r <- foldS(tail, a)(f)
    } yield r
  }

  def binOp(op: SExpr, a: SNumber, b: SNumber) =
    Eval(Cons(op, Cons(a, Cons(b, SNil))))
}
