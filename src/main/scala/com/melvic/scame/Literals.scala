package com.melvic.scame

object Literals {
  val TrueLiteral = "#t"
  val FalseLiteral = "#f"

  val DefineLiteral = "define"
  val QuoteLiteral = "quote"
  val LambdaLiteral = "lambda"

  val NilLiteral = "()"

  val NewLineLiteral = "newline"
  val TabLiteral = "tab"
  val SpaceLiteral = "space"
  val BackspaceLiteral = "backspace"

  val invalidSymbol = "\"\'() ".toList.map(_.toString) ++ List(DefineLiteral,
    QuoteLiteral, LambdaLiteral, TrueLiteral, FalseLiteral)
}
