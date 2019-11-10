package com.melvic.scame.show

import com.melvic.scame.ErrorCode._
import com.melvic.scame.{ErrorCode, SExpr}

// TODO: Report locations (or show stacktrace) of errors
trait ErrorCodeInstances {
  implicit val showErrorCode: Show[ErrorCode] = symbol =>
    "Error: " + (symbol match {
      case SymbolNotFound(name) => s"Unbound symbol: $name"
      case ExprMismatch(expected, got) =>
        // Note: If there are three items, this will result to a tricolon.
        // If there are more (which is less likely), this might become weird.
        val expectedString = expected.mkString(" or ")
        
        s"Expression mismatch. Expected: $expectedString. Got: ${Show[SExpr](got)}"
      case TooFewArguments(expectedMin, got) =>
        s"Too Few Arguments. Expected: at least $expectedMin. Got: $got"
      case TooManyArguments(expectedMax, got) =>
        s"Too Many Arguments. Expected: at most $expectedMax. Got: $got"
      case InvalidLambda => "Invalid Lambda"
      case NotAFunction(expr) => s"${Show[SExpr](expr)} is not a function"
      case SymbolAlreadyExists(symbol) => s"Symbol already exists: $symbol"
    })
}

object ErrorCodeInstances extends ErrorCodeInstances