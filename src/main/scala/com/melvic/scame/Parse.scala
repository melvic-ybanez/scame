package com.melvic.scame

import fastparse._
import ScriptWhitespace._
import com.melvic.scame.SExpr.{Define, Quote, _}
import Literals._

object Parse {
  def sTrue[_: P] = P(TrueLiteral).map(_ => STrue)
  def sFalse[_: P] = P(FalseLiteral).map(_ => SFalse)
  def boolean[_: P] = P(sTrue | sFalse)

  def integer[_: P] = P(CharsWhileIn("0-9").!).map(i => SInt(i.toInt))
  def rational[_: P] = P(integer ~ "/" ~ integer).map { case (SInt(n), SInt(d)) =>
    SRational(n, d)
  }
  def real[_: P] = P(integer ~ "." ~ integer).map { case (SInt(n), SInt(d)) =>
    SReal(n, d)
  }
  def number[_: P] = P(rational | real | integer)

  lazy val specialCharsMap = SpecialCharacters.map { case (k, v) => (v, k) }

  def specialCharParser[_: P](value: String) =
    P(s"#\\$value").map(_ => SChar(specialCharsMap(value)))

  def newline[_: P] = specialCharParser("newline")
  def tab[_: P] = specialCharParser("tab")
  def space[_: P] = specialCharParser("space")
  def backspace[_: P] = specialCharParser("backspace")

  def specialCharacter[_: P] = P(newline | tab | space | backspace)

  def regularChar[_: P] = P("#\\" ~ AnyChar.!).map(SChar)

  def character[_: P] = P(specialCharacter | regularChar)

  /**
   * Matches any name that starts with an underscore, dollar sign,
   * or alpha-character.
   * Note: This is subject to change in the future.
   */
  def symbol[_: P] = P((("_" | "$" | CharIn("a-zA-Z")) ~ AnyChar.rep(0)).!).map(SSymbol)

  def string[_: P] = P("\"" ~~ CharsWhile(_ != '\"').! ~~ "\"").map(
    _.toList.map(c => SChar(c.toString)).asSList)

  def define[_: P] = P(DefineLiteral).map(_ => Define)
  def quote[_: P] = P(QuoteLiteral).map(_ => Quote)
  def sLambda[_: P] = P(LambdaLiteral).map(_ => Lambda)

  def specialForm[_: P] = P(define | quote | sLambda)

  def sList[_: P] = P("(" ~ expression.rep(0) ~ ")").map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P(boolean | number | character | string | specialForm | sList | symbol)

  def apply(input: String) = parse(input, expression(_))
}
