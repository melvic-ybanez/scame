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
    val eval = atom orElse symbol orElse
      emptyList orElse pair orElse
      define orElse sLambda orElse
      quote
    eval(expr)
  }

  def atom: PartialEval = { case atom: Atom => atom.valid }

  // TODO: Special forms like define, symbol, etc should
  //  have a default value that displays "#<Syntax name-of-the-form>"
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
      ExprMismatch("pair" +: Vector(), tail.toString).invalid
  }

  def define: PartialEval = {
    case Cons(Define, Cons(SSymbol(name), SNil)) => Eval(Define(name, SNil))
    case Cons(Define, Cons(SSymbol(name), Cons(value, SNil))) => Eval(value).flatMap { v =>
      Eval(Define(name, v))
    }
    case Cons(Define, body) => ExprMismatch(
      Constants.Symbol +: Constants.Pair +: Vector(),
      body.toString).invalid
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
        case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h.toString).invalid
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

  def provideNameToEnv[A](name: String, env: ZIO[EnvConfig, ErrorCode, A]) =
    env.provideSome[EvalConfig](e => (name, e.env))

  def register(name: String, expr: SExpr) = provideNameToEnv(name, Env.register(expr))
}
