package com.melvic.scame

import com.melvic.scame.Env.{EnvConfig, ReadEnv}
import com.melvic.scame.ErrorCode.{ExprMismatch, IncorrectParamCount, SymbolNotFound}
import com.melvic.scame.Expr._
import zio.{IO, ZIO}

object Eval {
  type EvaluationE[E] = ZIO[EvalConfig, ErrorCode, E]
  type Evaluation = EvaluationE[Expr]
  type PartialEval = PartialFunction[Expr, Evaluation]

  final case class EvalConfig(expr: Expr, env: Env)

  def apply(expr: Expr, env: Env): Evaluation = Eval.apply.provide(EvalConfig(expr, env))

  def apply(expression: Expr): Evaluation =
    apply.provideSome[EvalConfig](_.copy(expr = expression))

  def apply: Evaluation = ZIO.accessM { case EvalConfig(expr, _) =>
    val eval = atom orElse symbol orElse
      emptyList orElse pair orElse
      define orElse sLambda orElse
      quote
    eval(expr)
  }

  def atom: PartialEval = { case atom: Atom => atom.valid }

  def symbol: PartialEval = { case Symbol(name) =>
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
    case Cons(Define, Cons(Symbol(name), SNil)) => Eval(Define(name, SNil))
    case Cons(Define, Cons(Symbol(name), Cons(value, SNil))) => Eval(value).flatMap { v =>
      Eval(Define(name, v))
    }
    case Cons(Define, body) => ExprMismatch(
      Constants.Symbol +: Constants.Pair +: Vector(),
      body.toString).invalid
    case Define(name, value) => register(name, value).map(Definition)
  }

 def sLambda: PartialEval = {
    case Cons(Lambda, Cons(params, body)) => Lambda(params, body).valid
    case Cons(Lambda(params: SList, body), args: SList) =>
      def recurse(env: Env): (SList, SList) => EvaluationE[Env] = {
        case (SNil, _) | (_, SNil) => ZIO.succeed(env)
        case (Cons(Symbol(param), t), Cons(arg, t1)) => for {
          evaluatedArg <- Eval.apply(arg)
          newEnv <- register(param, evaluatedArg)
          result <- recurse(newEnv)(t, t1)
        } yield result
        case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h.toString).invalid
      }

      for {
        config <- ZIO.environment[EvalConfig]
        env <- recurse(config.env)(params, args)
        result <- Eval.apply(body, env)
      } yield result

    // If the parameter is a symbol instead of a list, set it to the
    // whole argument list.
    case Cons(Lambda(Symbol(param), body), args) =>
      for {
        argList <- args.asScalaList.foldLeft[EvaluationE[SList]](SNil.valid) {
          case (acc, arg) => for {
            tail <- acc
            head <- Eval.apply(arg)
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

  def register(name: String, expr: Expr) = provideNameToEnv(name, Env.register(expr))
}
