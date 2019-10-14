package com.melvic.scame

import com.melvic.scame.Expr.{SBoolean, SChar, SFalse, STrue}

trait ShowInstances {
  implicit val showBoolean: Show[SBoolean] = {
    case STrue => Constants.TrueLiteral
    case SFalse => Constants.FalseLiteral
  }

  implicit val showCharacter: Show[SChar] = _.value

  implicit def showExpr(implicit boolean: Show[SBoolean],
      character: Show[SChar]): Show[Expr] = {
    case value: SBoolean => boolean(value)
    case value: SChar => character(value)
  }
}

object ShowInstances extends ShowInstances
