package com.melvic.scame.instances

import com.melvic.scame.ErrorCode._
import com.melvic.scame.{ErrorCode, SExpr, Show}

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
      case InvalidLambda => "Invalid Lambda"
    })
}

object ErrorCodeInstances extends ErrorCodeInstances