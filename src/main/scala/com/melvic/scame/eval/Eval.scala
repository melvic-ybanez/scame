package com.melvic.scame.eval

import com.melvic.scame.Env.NonEmptyEnv
import com.melvic.scame._
import com.melvic.scame.errors.ErrorCode._
import com.melvic.scame.exprs.SExpr
import com.melvic.scame.exprs.SExpr._
import com.melvic.scame.utils.SMath
import zio.ZIO

object Eval {
  def apply(expr: SExpr, env: Env): Evaluation = apply.provide(EvalConfig(expr, env))

  def apply(expression: SExpr): Evaluation =
    apply.provideSome[EvalConfig](_.copy(expr = expression))

  def apply: Evaluation = ZIO.accessM { case EvalConfig(expr, _) =>
    val eval = atom orElse symbol orElse specialForms orElse builtInFunctions orElse sList
    eval(expr)
  }

  def atom: PartialEval = { case atom: Atom => atom.! }

  def symbol: PartialEval = { case SSymbol(name) =>
    for {
      config <- ZIO.environment[EvalConfig]
      sexpr <- Env.globalSearch.provide((name, config.env))
    } yield sexpr
  }

  def sList: PartialEval = {
    case SNil => SNil.!
    case expr :: args => for {
      func <- Eval(expr)
      result <- callFunction(func :: args)
    } yield result
  }

  def specialForms: PartialEval = define orElse
    cond orElse let  orElse quote orElse sLambda

  def define: PartialEval = {
    case Define :: SSymbol(name) :: SNil => Eval(Define(name, SNil))
    case Define :: SSymbol(name) :: value :: SNil => Eval(value).flatMap { v =>
      Eval(Define(name, v))
    }
    case Define :: body => ExprMismatch(
      Constants.Symbol +: Constants.Pair +: Vector(), body).!
    case Define(name, value) => register(name, value).map(Definition)
  }

  def sLambda: PartialEval = {
    // A lambda expression requires a body
    case Lambda :: _ :: SNil => InvalidLambda.!

    // Construct a lambda object. Both the params and the body shouldn't
    // be evaluated.
    case Lambda :: params :: body => Lambda(params, body).!
  }

  def callFunction: PartialEval = {
    case Lambda(params: SList, body :: _) :: args =>
      /**
       * Binds every parameter to its corresponding argument.
       */
      def assignArgs(env: Env): (SList, SList) => EvaluationE[Env] = {
        case (SNil, _) | (_, SNil) => ZIO.succeed(env)
        case (SSymbol(param) :: symbols, arg :: args) => for {
          evaluatedArg <- Eval(arg)
          newEnv <- Env.register(evaluatedArg).provide((param, env))
          result <- assignArgs(newEnv)(symbols, args)
        } yield result
        case (h :: _, _) => ExprMismatch(Vector(Constants.Symbol), h).!
      }

      for {
        config <- ZIO.environment[EvalConfig]
        env <- assignArgs(config.env)(params, args)
        result <- Eval(body, env)
      } yield result

    // If the parameter is a symbol instead of a list, set it to the
    // whole argument list.
    case Lambda(SSymbol(param), body) :: args =>
      for {
        argList <- args.asScalaList.foldLeft[EvaluationE[SList]](SNil.!) { (acc, arg) =>
          for {
            tail <- acc
            head <- Eval(arg)
          } yield head :: tail
        }
        env <- register(param, argList)
        result <- Eval(body, env)
      } yield result

    case expr :: _ => NotAFunction(expr).!
  }

  def quote: PartialEval = {
    case Quote :: arg :: _ => arg.!
  }

  def cond: PartialEval = {
    case Cond :: (pred :: body :: _) :: _ if !SExpr.falsy(pred) => Eval(body)
    case Cond :: (pred :: _) :: SNil if SExpr.falsy(pred) => SNil.!
    case Cond :: (pred :: _) :: rest if SExpr.falsy(pred) => Eval(Cond :: rest)
    case Cond :: (sexpr :: body) :: rest => for {
      head <- Eval(sexpr)
      result <- Eval(Cond :: (head :: body) :: rest)
    } yield result
    case Cond :: expr => ExprMismatch(Vector(s"List of ${Constants.Pair}s"), expr).!
  }

  /**
   * A let expression ideally takes two arguments: the list of symbol-value pairs
   * and the main body of computation.
   */
  def let: PartialEval = {
    // If the head of the first argument is a list, see if you can make
    // a definition out of it by delegating to the `define` special form.
    case Let :: ((define: SList) :: defineRest) :: body => for {
      definedHead <- Eval(Define :: define)
      result <- definedHead match {
        case Definition(env) => Eval(Let :: defineRest :: body, env)
      }
    } yield result

    // If there aren't any definitions to evaluate, evaluate the body.
    case Let :: SNil :: body :: SNil => Eval(body)

    case Let :: expr => ExprMismatch(Vector("A list of pairs for bindings"), expr).!
  }

  def builtInFunctions: PartialEval =
    arithmetic orElse relational orElse equalities orElse cons orElse
      listFunc orElse sNull orElse car orElse cdr

  def cons: PartialEval = requireArgsCount(Cons, 2) {
    case Cons :: head :: tail :: SNil => for {
      h <- Eval(head)
      t <- Eval(tail)
    } yield t match {
      // If the tail is a list, the whole cons is a proper list.
      case tList: SList => h :: tList
      // Otherwise, it remains an improper list.
      case _ => Pair(h, t)
    }
  }

  def listFunc: PartialEval = {
    case SList :: SNil => SNil.!

    // TODO: Optimize this
    case SList :: args => foldS(args.asScalaList.reverse.asSList, SNil) {
      case (acc: SList, expr) => (expr :: acc).!
    }
  }

  def arithmetic: PartialEval = {
    def add: PartialEval = {
      case Add :: args => foldS(args, SInt(0)) {
        case (SInt(a), SInt(b)) => SInt(a + b).!
        case (SInt(a), SRational(n, d)) => SRational(a * d + n, d).!
        case (SInt(a), SReal(b)) => SReal(a + b).!
        case (SRational(n1, d1), SRational(n2, d2)) =>
          val lcd = SMath.lcm(d1, d2)
          SRational(lcd / d1 * n1 + lcd / d2 * n2, lcd).!
        case (rat: SRational, real: SReal) =>
          // evaluate the two as real numbers
          binOp(Add, real, SMath.rationalToReal(rat))
        case (SReal(a), SReal(b)) => SReal(a + b).!
        case pair => reverseBinOp(Add)(pair)
      }
    }

    /**
     * Computes the difference. If there are more than one numbers,
     * negate all of them but the first one, and compute their sum.
     */
    def subtract: PartialEval = reverseOp(Subtract, Add)(negate)

    def multiply: PartialEval = {
      case Multiply :: args => foldS(args, SInt(1)) {
        case (SInt(a), SInt(b)) => SInt(a * b).!
        case (SInt(a), SRational(n, d)) => SRational(a * n, d).!
        case (SInt(a), SReal(b)) => SReal(a * b).!
        case (SRational(n, d), SRational(n1, d1)) =>
          // multiply the numerators and denominators
          val num = n * n1
          val denom = d * d1

          // simplify the resulting fraction
          val gcd = BigInt(num).gcd(BigInt(denom)).intValue
          SRational(num / gcd, denom / gcd).!
        case (rat: SRational, real: SReal) =>
          binOp(Multiply, real, SMath.rationalToReal(rat))
        case (SReal(a), SReal(b)) => SReal(a * b).!
        case pair => reverseBinOp(Multiply)(pair)
      }
    }

    /**
     * Computes the quotient. If there are more than one numbers,
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
      case `op` :: SNil => TooFewArguments(1, 0).!
      case `op` :: (n: SNumber) :: SNil => f(n).!
      case `op` :: (h: SNumber) :: t => for {
        reverse <- foldS(t, SNil) {
          case (acc: SList, n: SNumber) => (f(n) :: acc).!
          case (_, n) => nonNumber(n)
        }

        // Only accepts lists. We have to manually pattern match
        // because foldS is weakly typed.
        reversedNumbers <- reverse match {
          case l: SList => l.!
          case e => nonNumber(e)
        }

        diff <- Eval(op1 :: h :: reversedNumbers)
      } yield diff
      case `op` :: expr :: _ => nonNumber(expr)
    }

    def negate: SNumber => SNumber = {
      case SInt(a) => SInt(-a)
      case SRational(n, d) => SRational(-n, d)
      case SReal(n) => SReal(-n)
    }

    // TODO: Simplify resulting rationals to avoid awkward values
    //  such as 1/1.
    add orElse subtract orElse multiply orElse divide
  }

  def relational: PartialEval = {
    def evalRelational(op: Relational)(f: (Double, Double) => Boolean): PartialEval =
      requireMinArgsCount(op, 2) {
        case `op` :: h :: t =>
          def result(number: SNumber, p: => Boolean) =
            if (p) number.! else Return(SFalse).!

          boolFunc(t, h) {
            case (SInt(a), i @ SInt(b)) => result(i, f(a, b))
            case (SInt(a), r @ SRational(n, d)) => result(r, f(a, n / d))
            case (SInt(a), r @ SReal(n)) => result(r, f(a, n))
            case (SRational(n, d), r @ SRational(n1, d1)) => result(r, f(n / d, n1 / d1))
            case (SRational(n, d), sr @ SReal(r)) => result(sr, f(n / d, r))
            case (SReal(a), r @ SReal(b)) => result(r, f(a, b))
            case pair => reverseBinOp(`op`)(pair)
          }
      }

    def eqSign = evalRelational(EqSign)(_ == _)
    def greater = evalRelational(GT)(_ > _)
    def greaterEqual = evalRelational(GTE)(_ >= _)
    def less = evalRelational(LT)(_ < _)
    def lessEqual = evalRelational(LTE)(_ <= _)

    eqSign orElse greater orElse greaterEqual orElse less orElse lessEqual
  }

  def equalities: PartialEval = {
    def equality(op: SExpr)(f: (SExpr, SExpr) => Boolean): PartialEval =
      requireMinArgsCount(op, 2) {
        case `op` :: arg :: args => boolFunc(args, arg) {
          // Symbols should be equal if they have the same name
          // because symbols are unique
          case (s @ SSymbol(a), SSymbol(b)) if a == b => s.!

          case (a, b) if f(a, b) => a.!
          case _ => Return(SFalse).!
        }
      }

    def sEq = equality(Eq)(_ eq _)
    def sEqual = equality(Equal)(_ == _)

    sEq orElse sEqual
  }

  def sNull: PartialEval = requireArgsCount(Null, 1) {
    case Null :: SNil :: SNil => STrue.!
    case Null :: _ => SFalse.!
  }

  def car: PartialEval = requireNonEmptyList(Car) { case h :: _ => h.! }

  def cdr: PartialEval = requireNonEmptyList(Cdr) { case _ :: t => t.! }
}
