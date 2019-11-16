package com.melvic.scame.errors

import com.melvic.scame.exprs.SExpr

import scala.annotation.tailrec

final case class IndexedSExpr(expr: SExpr, position: Int, children: List[IndexedSExpr])

// TODO: Add support for multi-modules
object IndexedSExpr {
  type StackTrace = List[Int]

  def apply(expr: SExpr, pos: Int) = IndexedSExpr(expr, pos, Nil)

  def find(indexedSExpr: IndexedSExpr, expr: SExpr, stackTrace: StackTrace): StackTrace =
    indexedSExpr match {
      case IndexedSExpr(expr1, pos, _) if expr == expr1 => pos :: stackTrace
      case IndexedSExpr(_, _, Nil) => stackTrace
      case IndexedSExpr(_, _, children) =>
        @tailrec
        def recurse(children: List[IndexedSExpr]): StackTrace = children match {
          case Nil => Nil
          case child :: restOfChildren =>
            val result = IndexedSExpr.find(child, expr, Nil)
            if (result.isEmpty) recurse(restOfChildren) else result
        }
        recurse(children) ++ stackTrace
    }
}
