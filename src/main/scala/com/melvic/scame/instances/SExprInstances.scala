package com.melvic.scame.instances

import com.melvic.scame.Literals.{FalseLiteral, TrueLiteral}
import com.melvic.scame.SExpr._
import com.melvic.scame.{Literals, SExpr, Show}

trait SExprInstances {
  implicit val showBoolean: Show[SBoolean] = {
    case STrue => TrueLiteral
    case SFalse => FalseLiteral
  }

  implicit val showNumber: Show[SNumber] = {
    case SInt(value) => value.toString
    case SRational(num, denom) => s"$num/$denom"
    case SReal(whole, fractional) => s"$whole.$fractional"
  }

  implicit def showSExpr(implicit showBoolean: Show[SBoolean], showNumber: Show[SNumber]): Show[SExpr] = {
    case boolean: SBoolean => showBoolean(boolean)
    case SChar(value) => value
    case number: SNumber => showNumber(number)
    case SSymbol(name) => name
    case _: Definition => Literals.NilLiteral
    case expr: SList =>
      val showExpr = showSExpr
      val exprs = expr.asScalaList.map(expr => showExpr(expr)).mkString(" ")
      s"($exprs)"
  }
}
