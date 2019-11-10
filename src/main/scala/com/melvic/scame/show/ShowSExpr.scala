package com.melvic.scame.show

import com.melvic.scame.Literals.{FalseLiteral, TrueLiteral}
import com.melvic.scame.SExpr._
import com.melvic.scame.{Literals, SExpr}

trait ShowSExpr {
  implicit val showBoolean: Show[SBoolean] = {
    case STrue => TrueLiteral
    case SFalse => FalseLiteral
  }

  implicit val showNumber: Show[SNumber] = {
    case SInt(value) => value.toString
    case SRational(num, denom) => s"$num/$denom"
    case SReal(value) => value.toString
  }

  implicit val showSpecialForm: Show[SpecialForm] = { expr =>
    s"#<Syntax ${expr.toString.toLowerCase}>"
  }

  implicit val showFunction: Show[SFunction] = { function =>
    val functionMap: Map[SFunction, String] = (Literals.ArithmeticMap ++ Literals.RelationalMap)
      .map { case (k, v) => (v, k) }
    s"#<Function ${functionMap(function)}>"
  }

  implicit def showPair(implicit showExpr: Show[SExpr]): Show[Pair] = {
    case Pair(a, b: Pair) => s"${showExpr(a)} ${showPair(showExpr)(b)}"
    case Pair(a, b) => s"${showExpr(a)} . ${showExpr(b)}"
  }

  implicit def showSExpr(implicit showBoolean: Show[SBoolean],
      showNumber: Show[SNumber], showSpecialForm: Show[SpecialForm],
      showFunction: Show[SFunction]): Show[SExpr] = {
    case boolean: SBoolean => showBoolean(boolean)
    case SChar(value) => value
    case number: SNumber => showNumber(number)
    case SSymbol(name) => name
    case specialForm: SpecialForm => showSpecialForm(specialForm)
    case function: SFunction => showFunction(function)
    case Lambda(_, _) => "#<Closure>"
    case _: Definition => Literals.NilLiteral
    case expr: SList =>
      val showExpr = showSExpr
      val exprs = expr.asScalaList.map(expr => showExpr(expr)).mkString(" ")
      s"($exprs)"
    case pair: Pair => s"(${showPair(showSExpr)(pair)})"
  }
}
