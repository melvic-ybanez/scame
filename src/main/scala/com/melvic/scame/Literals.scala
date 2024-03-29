package com.melvic.scame

import com.melvic.scame.SExpr._

object Literals {
  val TrueLiteral = "#t"
  val FalseLiteral = "#f"

  val DefineLiteral = "define"
  val QuoteLiteral = "quote"
  val LambdaLiteral = "lambda"
  val CondLiteral = "cond"
  val LetLiteral = "let"

  val NilLiteral = "()"

  val NewLineLiteral = "newline"
  val TabLiteral = "tab"
  val SpaceLiteral = "space"
  val BackspaceLiteral = "backspace"

  val invalidSymbol = "\"\'() "

  val ArithmeticMap = Map("+" -> Add,
    "-" -> Subtract, "*" -> Multiply, "/" -> Divide)

  val RelationalMap = Map("=" -> Equal,
    ">" -> GT, ">=" -> GTE, "<" -> LT, "<=" -> LTE)
}
