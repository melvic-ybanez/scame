package com.melvic.scame

import com.melvic.scame.ErrorCode.SymbolNotFound

trait ErrorCodeInstances {
  implicit val showSymbolNotFound: Show[SymbolNotFound] = symbol =>
    s"Unbound symbol: ${symbol.name}"

  implicit def showErrorCode(implicit showSymbolNotFound: Show[SymbolNotFound]): Show[ErrorCode] = symbol =>
    "Error: " + (symbol match {
      case symbolNotFound: SymbolNotFound => showSymbolNotFound(symbolNotFound)
    })
}

object ErrorCodeInstances extends ErrorCodeInstances