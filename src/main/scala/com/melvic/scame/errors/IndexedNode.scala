package com.melvic.scame.errors

import com.melvic.scame.exprs.SExpr

final case class IndexedNode(expr: SExpr, pos: Int, children: List[IndexedNode])

// TODO: Add support for multi-modules
object IndexedNode {
  def apply(expr: SExpr, pos: Int) = IndexedNode(expr, pos, Nil)
}
