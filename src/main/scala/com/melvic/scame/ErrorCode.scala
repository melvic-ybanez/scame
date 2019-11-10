package com.melvic.scame

trait ErrorCode

object ErrorCode {
  final case class SymbolNotFound(name: String) extends ErrorCode
  final case class ExprMismatch(expected: Vector[String], got: SExpr) extends ErrorCode
  final case class SymbolAlreadyExists(name: String) extends ErrorCode
  final case class IncorrectParamCount(expected: Int, got: Int) extends ErrorCode
  final case class TooFewArguments(expectedMin: Int, got: Int) extends ErrorCode
  final case class TooManyArguments(expectedMax: Int, got: Int) extends ErrorCode
  final case class NotAFunction(expr: SExpr) extends ErrorCode
  case object InvalidLambda extends ErrorCode
}
