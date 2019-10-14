package com.melvic.scame

import com.melvic.scame.Eval.EvalResult
import com.melvic.scame.Expr.{Cons, SList, SNil}

import scala.annotation.tailrec

trait Implicits {
  implicit class ExprAsResult[E <: Expr](expr: E) {
    def valid: EvalResult = Right(expr)
  }

  implicit class ErrorCodeAsResult[E <: ErrorCode](error: E) {
    def invalid[A]: ErrorOr[A] = Left(error)
  }

  implicit class SListToScalaList(list: SList) {
    def asScalaList: List[Expr] = {
      @tailrec
      def recurse(sList: SList, acc: List[Expr]): List[Expr] = sList match {
        case SNil => acc
        case Cons(head, tail) => recurse(tail, head :: acc)
      }

      recurse(list, Nil)
    }
  }
}

object Implicits extends Implicits
