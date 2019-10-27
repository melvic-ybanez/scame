package com.melvic.scame

import com.melvic.scame.Env.EnvConfig
import com.melvic.scame.ErrorCode.{ExprMismatch, IncorrectParamCount}
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

  def atom: PartialEval = { case atom: Atom => atom.valid }

  def symbol: PartialEval = { case SSymbol(name) =>
    provideNameToEnv(name, Env.globalSearch)
  }

  def emptyList: PartialEval = { case SNil => SNil.valid }

  def pair: PartialEval = {
    case Cons(Cons, Cons(_, SNil)) => IncorrectParamCount(2, 1).invalid
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
      ExprMismatch("pair" +: Vector(), tail).invalid
  }

  def define: PartialEval = {
    case Cons(Define, Cons(SSymbol(name), SNil)) => Eval(Define(name, SNil))
    case Cons(Define, Cons(SSymbol(name), Cons(value, SNil))) => Eval(value).flatMap { v =>
      Eval(Define(name, v))
    }
    case Cons(Define, body) => ExprMismatch(
      Constants.Symbol +: Constants.Pair +: Vector(), body).invalid
    case Define(name, value) => register(name, value).map(Definition)
  }

  def sLambda: PartialEval = {
    // Construct a lambda object. Both the params and the body shouldn't
    // be evaluated.
    case Cons(Lambda, Cons(params, body)) => Lambda(params, body).valid

    case Cons(Lambda(params: SList, body), args: SList) =>
      def recurse(env: Env): (SList, SList) => EvaluationE[Env] = {
        case (SNil, _) | (_, SNil) => ZIO.succeed(env)
        case (Cons(SSymbol(param), t), Cons(arg, t1)) => for {
          evaluatedArg <- Eval(arg)
          newEnv <- register(param, evaluatedArg)
          result <- recurse(newEnv)(t, t1)
        } yield result
        case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h).invalid
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
        argList <- args.asScalaList.foldLeft[EvaluationE[SList]](SNil.valid) { (acc, arg) =>
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
    case Cons(Quote, Cons(arg, _)) => arg.valid
  }

  def cond: PartialEval = {
    case Cons(Cond, Cons(Cons(pred, Cons(body, _)), _)) if !SExpr.falsy(pred) => Eval(body)
    case Cons(Cond, Cons(Cons(pred, _), SNil)) if SExpr.falsy(pred) => SNil.valid
    case Cons(Cond, Cons(Cons(pred, _), rest)) if SExpr.falsy(pred) => Eval(Cons(Cond, rest))
    case Cons(Cond, Cons(Cons(sexpr, body), rest)) => for {
      head <- Eval(sexpr)
      result <- Eval(Cons(Cond, Cons(Cons(head, body), rest)))
    } yield result
    case Cons(Cond, expr) => ExprMismatch(Vector(s"List of ${Constants.Pair}s"), expr).invalid
  }

  /**
   * A let expression ideally takes two arguments: the list of symbol-value pairs
   * and the main body of computation.
   */
  def let: PartialEval = {
    // If the head of the first argument is a non-empty list, see if you can make
    // a definition out of it by delegating to the define special form.
    case Cons(Let, Cons(Cons(define: Cons, defineRest), body)) => for {
      definedHead <- Eval(Cons(Define, define))
      result <- definedHead match {
        case Definition(env) => Eval(Cons(Let, Cons(defineRest, body)), env)
      }
    } yield result

    // If there aren't any definitions to evaluate, evaluate the body.
    case Cons(Let, Cons(SNil, Cons(body, SNil))) => Eval(body)

    case Cons(Let, expr) => ExprMismatch(Vector("A list of pairs for bindings"), expr).invalid
  }

  def arithmetic: PartialEval = {
    case Cons(Add, args: SList) => fold(args, SInt(0)) {
      case (SInt(a), SInt(b)) => SInt(a + b).valid
      case (SInt(a), SRational(n, d)) => SRational(a * d + n, d).valid
      case (SInt(a), SReal(b)) => SReal(a + b).valid
      case (r: SRational, i: SInt) => compute(Add, i, r)    // reverse
      case (SRational(n1, d1), SRational(n2, d2)) =>
        val lcd = Utils.lcm(d1, d2)
        SRational(lcd / d1 * n1 + lcd / d2 * n2, lcd).valid
      case (SRational(n, d), r: SReal) =>
        // convert the rational to real
        val dec = n.toDouble / d
        val r1 = SReal(dec)

        // evaluate the two real numbers
        compute(Add, r, r1)
      case (r: SReal, i: SInt) => compute(Add, i, r)    // reverse
      case (r: SReal, f: SRational) => compute(Add, f, r)   // reverse
      case (SReal(a), SReal(b)) => SReal(a + b).valid
    }
  }

  def provideNameToEnv[A](name: String, env: ZIO[EnvConfig, ErrorCode, A]) =
    env.provideSome[EvalConfig](e => (name, e.env))

  def register(name: String, expr: SExpr) = provideNameToEnv(name, Env.register(expr))

  /**
   * Short-circuiting fold designed specifically for evaluated s-expressions.
   * TODO: This is not tail-recursive
   */
  def fold(sList: SList, acc: SExpr)(f: (SExpr, SExpr) => Evaluation): Evaluation = sList match {
    case SNil => acc.valid
    case Cons(head, tail) => for {
      a <- f(acc, head)
      r <- fold(tail, a)(f)
    } yield r
  }

  def compute(op: Arithmetic, a: SNumber, b: SNumber) =
    Eval(Cons(op, Cons(a, Cons(b, SNil))))
}
