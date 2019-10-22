package com.melvic.scame

import com.melvic.scame.Literals.{FalseLiteral, SpecialCharacters, TrueLiteral}
import com.melvic.scame.SExpr.{SBoolean, SChar, SFalse, SInt, SList, SNumber, SRational, SReal, STrue}

trait SExprInstances {
  implicit val showBoolean: Show[SBoolean] = {
    case STrue => TrueLiteral
    case SFalse => FalseLiteral
  }

  implicit val showCharacter: Show[SChar] = { char =>
    val value = SpecialCharacters.getOrElse(char.value, char.value)
    s"#\\$value"
  }

  implicit val showInteger: Show[SInt] = _.value.toString
  implicit val showRational: Show[SRational] = { case SRational(num, denom) =>
    s"$num/$denom"
  }
  implicit val showReal: Show[SReal] = { case SReal(whole, fractional) =>
    s"$whole.$fractional"
  }
  implicit def showNumber(implicit showInt: Show[SInt],
      showRational: Show[SRational], showReal: Show[SReal]): Show[SNumber] = {
    case int: SInt => showInt(int)
    case rat: SRational => showRational(rat)
    case real: SReal => showReal(real)
  }

  implicit def showSExpr(implicit showBoolean: Show[SBoolean],
      showCharacter: Show[SChar], showNumber: Show[SNumber]): Show[SExpr] = {
    case boolean: SBoolean => showBoolean(boolean)
    case char: SChar => showCharacter(char)
    case number: SNumber => showNumber(number)
    case expr: SList =>
      val showExpr = showSExpr
      val exprs = expr.asScalaList.map(expr => showExpr(expr)).mkString(" ")
      s"($exprs)"
  }
}
