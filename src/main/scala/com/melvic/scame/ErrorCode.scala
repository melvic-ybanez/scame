package com.melvic.scame

trait ErrorCode

object ErrorCode {
  final case class SymbolNotFound(name: String) extends ErrorCode
  final case class ExprMismatch(expected: Vector[String], got: String) extends ErrorCode
  final case class SymbolAlreadyExists(name: String) extends ErrorCode
  final case class IncorrectParamCount(expected: Int, got: Int) extends ErrorCode
}
