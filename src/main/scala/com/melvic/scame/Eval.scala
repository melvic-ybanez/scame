package com.melvic.scame

import com.melvic.scame.Env.{EnvConfig, ReadEnv}
import com.melvic.scame.ErrorCode.{ExprMismatch, IncorrectParamCount, SymbolNotFound}
import com.melvic.scame.Expr._
import zio.{IO, ZIO}

object Eval {
  type Evaluation = ZIO[EvalConfig, ErrorCode, Expr]
  type PartialEval = PartialFunction[Expr, Evaluation]

  final case class EvalConfig(expr: Expr, env: Env)

  def apply(expr: Expr, env: Env) = apply.provide(EvalConfig(expr, env))

  def apply(expression: Expr) =
    apply.provideSome[EvalConfig](_.copy(expr = expression))

  def apply: Evaluation = ZIO.accessM { case EvalConfig(expr, _) =>
    val eval = atom orElse symbol orElse
      emptyList orElse pair orElse
      define orElse lambda orElse
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

  def lambda: PartialEval = {
    case Cons(Lambda, Cons(params, body)) => Lambda(params, body).valid
    case Cons(Lambda(params: SList, body), args: SList) =>
      def recurse(env: Env): (SList, SList) => ZIO[EvalConfig, ErrorCode, Env] = {
        case (SNil, _) | (_, SNil) => ZIO.succeed(env)
        case (Cons(Symbol(param), t), Cons(arg, t1)) => for {
          a <- Eval(arg)
          e <- register(param, a)
          r <- recurse(e)(t, t1)
        } yield r
        case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h.toString).invalid
      }

      for {
        ec <- ZIO.environment[EvalConfig]
        e <- recurse(ec.env)(params, args)
        b <- Eval(body, e)
      } yield b

    // If the parameter is a symbol instead of a list, set it to the
    // whole argument list.
    case Cons(Lambda(Symbol(param), body), args) =>
      for {
        a <- args.asScalaList.foldLeft[ZIO[EvalConfig, ErrorCode, SList]](SNil.valid) {
          case (acc, arg) => for {
            t <- acc
            h <- Eval(arg)
          } yield Cons(h, t)
        }
        e <- register(param, a)
        b <- Eval(body, e)
      } yield b
  }

  def quote: PartialEval = {
    case Cons(Quote, Cons(arg, _)) => arg.valid
  }

  def provideNameToEnv[Err <: ErrorCode, A](name: String, env: ZIO[EnvConfig, Err, A] ) =
    env.provideSome[EvalConfig](e => (name, e.env))

  def register(name: String, expr: Expr) = provideNameToEnv(name, Env.register(expr))
}
