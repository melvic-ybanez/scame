package com.melvic.scame.instances

import com.melvic.scame.ErrorCode.{ExprMismatch, SymbolNotFound}
import com.melvic.scame.{ErrorCode, SExpr, Show}

trait ErrorCodeInstances {
  implicit val showErrorCode: Show[ErrorCode] = symbol =>
    "Error: " + (symbol match {
      case SymbolNotFound(name) => s"Unbound symbol: $name"
      case ExprMismatch(expected, got) =>
        s"Expression mismatch. Expected: $expected. Got: ${Show[SExpr](got)}"
    })
}

object ErrorCodeInstances extends ErrorCodeInstances