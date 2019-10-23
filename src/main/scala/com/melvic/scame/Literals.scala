package com.melvic.scame

object Literals {
  val TrueLiteral = "#t"
  val FalseLiteral = "#f"

  val DefineLiteral = "define"
  val QuoteLiteral = "quote"
  val LambdaLiteral = "lambda"

  val NilLiteral = "()"

  val SpecialCharacters = Map("\n" -> "newline",
    "\t" -> "tab", " " -> "space", "\b" -> "backspace")

  val invalidSymbol = "\"\'() ".toList.map(_.toString) ++ List(DefineLiteral,
    QuoteLiteral, LambdaLiteral, TrueLiteral, FalseLiteral)
}
