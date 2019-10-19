package com.melvic.scame

import com.melvic.scame.Expr.{SBoolean, SChar, SFalse, STrue}
import Literals._

trait ShowInstances {
  implicit val showBoolean: Show[SBoolean] = {
    case STrue => TrueLiteral
    case SFalse => FalseLiteral
  }

  implicit val showCharacter: Show[SChar] = { char =>
    val value = SpecialCharacters.getOrElse(char.value, char.value)
    s"#\\$value"
  }

  implicit def showExpr(implicit boolean: Show[SBoolean],
      character: Show[SChar]): Show[Expr] = {
    case value: SBoolean => boolean(value)
    case value: SChar => character(value)
  }
}

object ShowInstances extends ShowInstances
