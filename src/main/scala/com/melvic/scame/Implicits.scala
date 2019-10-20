package com.melvic.scame

import com.melvic.scame.SExpr.{Cons, SList, SNil}
import zio.{IO, ZIO}

import scala.annotation.tailrec

trait Implicits {
  implicit class ExprAsResult[E <: SExpr](expr: E) {
    def valid = ZIO.succeed(expr)
  }

  implicit class ErrorCodeAsResult[E <: ErrorCode](error: E) {
    def invalid = ZIO.fail(error)
  }

  implicit class SListToScalaList(list: SList) {
    def asScalaList: List[SExpr] = {
      @tailrec
      def recurse(sList: SList, acc: List[SExpr]): List[SExpr] = sList match {
        case SNil => acc
        case Cons(head, tail) => recurse(tail, head :: acc)
      }

      recurse(list, Nil)
    }
  }
}

object Implicits extends Implicits
