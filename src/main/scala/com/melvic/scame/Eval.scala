package com.melvic.scame

import com.melvic.scame.ErrorCode.{ExprMismatch, SymbolNotFound}
import com.melvic.scame.Expr._

import scala.annotation.tailrec

object Eval {
  type EvalResult = ErrorOr[Expr]
  type PartialEval = PartialFunction[Expr, EvalResult]

  def apply(expr: Expr)(implicit env: Env): EvalResult = {
    def atom: PartialEval = { case atom: Atom => atom.valid }

    def symbol: PartialEval = { case Symbol(name) =>
      env(name).toRight(SymbolNotFound(name))
    }

    def emptyList: PartialEval = { case SNil => SNil.valid }

    def pair: PartialEval = {
      case Cons(Cons, tail) =>
        ExprMismatch("pair" +: Vector(), tail.toString).invalid
      case Cons(Cons, Cons(head, tail)) => for {
        h <- Eval(head)
        t <- Eval(tail)
      } yield t match {
          // If the tail is a list, the whole cons is a proper list.
        case tList: SList => Cons(h, tList)
          // Otherwise, it remains an improper list.
        case _ => Pair(h, t)
      }
    }

    def define: PartialEval = {
      case Cons(Define, Cons(Symbol(name), SNil)) => Eval(Define(name, SNil))
      case Cons(Define, Cons(Symbol(name), Cons(value, SNil))) => Eval(value).flatMap { v =>
        Eval(Define(name, v))
      }
      case Cons(Define, body) => ExprMismatch(
        Constants.Symbol +: Constants.Pair +: Vector(),
        body.toString).invalid
      case Define(name, value) => (env + (name, value)).map(Definition)
    }

    def lambda: PartialEval = {
      case Cons(Lambda, Cons(params, body)) => Lambda(params, body).valid
      case Cons(Lambda(params: SList, body), args: SList) =>
        def recurse(env: Env): (SList, SList) => ErrorOr[Env] = {
          case (SNil, _) | (_, SNil) => Right(env)
          case (Cons(Symbol(param), t), Cons(arg, t1)) => for {
            a <- Eval(arg)
            e <- env + (param, a)
            r <- recurse(e)(t, t1)
          } yield r
          case (Cons(h, _), _) => ExprMismatch(Vector(Constants.Symbol), h.toString).invalid
        }

        for {
          e <- recurse(env)(params, args)
          b <- Eval(body)(e)
        } yield b
      case Cons(Lambda(Symbol(param), body), args) => for {
        a <- args.asScalaList.foldLeft[EvalResult] (SNil.valid) {
          case (err: ErrorCode, _) => err
          case (_, arg) => Eval(arg)
        }
        e <- env + (param, a)
        b <- Eval(body)(e)
      } yield b
    }

    val eval = atom orElse symbol orElse
      emptyList orElse pair orElse
      define orElse lambda
    eval(expr)
  }
}
