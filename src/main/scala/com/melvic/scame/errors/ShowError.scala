package com.melvic.scame.errors

import com.melvic.scame.errors.ErrorCode._
import com.melvic.scame.exprs.SExpr
import com.melvic.scame.utils.Show

// TODO: Report locations (or show stacktrace) of errors
trait ShowError {
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

object ShowError extends ShowError