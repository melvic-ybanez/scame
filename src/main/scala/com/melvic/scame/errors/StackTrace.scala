package com.melvic.scame.errors

import com.melvic.scame.exprs.SExpr

sealed trait StackTrace

// TODO: Add support for multi-modules
object StackTrace {
  case object EmptyStack extends StackTrace

  final case class NonEmptyStackTrace(expr: SExpr, pos: Int, children: List[StackTrace]) extends StackTrace

  def apply(expr: SExpr, pos: Int) = NonEmptyStackTrace(expr, pos, Nil)

  def add(stackTrace: StackTrace, expr: SExpr, pos: Int): StackTrace = stackTrace match {
    case EmptyStack => StackTrace(expr, pos)
    case st @ NonEmptyStackTrace(_, _, children) => st.copy(
      children = StackTrace(expr, pos) :: children)
  }
}
