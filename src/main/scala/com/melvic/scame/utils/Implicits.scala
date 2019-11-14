package com.melvic.scame.utils

import com.melvic.scame.errors.ErrorCode
import com.melvic.scame.exprs.SExpr
import com.melvic.scame.exprs.SExpr.{SList, SNil, ::}
import zio.ZIO

import scala.annotation.tailrec

trait Implicits {
  implicit class ExprOps[E <: SExpr](expr: E) {
    def ! = ZIO.succeed(expr)
  }

  implicit class ErrorCodeOps[E <: ErrorCode](error: E) {
    def ! = ZIO.fail(error)
  }

  // TODO: We might want to just add the Scala list operations directly
  //  as extension methods instead of calling asScalaList, because it's
  //  possible that we will drop the conversion of SList to Scala list
  //  for performance reasons in the future.
  implicit class SListOps(list: SList) {
    lazy val size: Int = asScalaList.length

    def asScalaList: List[SExpr] = {
      @tailrec
      def recurse(sList: SList, acc: List[SExpr]): List[SExpr] = sList match {
        case SNil => acc
        case head :: tail => recurse(tail, head :: acc)
      }

      recurse(list, Nil).reverse
    }
  }

  implicit class ScalaListToSList(list: List[SExpr]) {
    def asSList: SList = list.reverse.foldLeft[SList](SNil) { (acc, expr) =>
      expr :: acc
    }
  }
}
