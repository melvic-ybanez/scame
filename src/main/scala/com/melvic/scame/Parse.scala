package com.melvic.scame

import fastparse._
import ScriptWhitespace._
import com.melvic.scame.SExpr._
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

  def string[_: P] = P("\"" ~ AnyChar.rep(0).! ~ "\"").map(
    _.toList.map(c => SChar(c.toString)).asSList)

  def expression[_: P]: P[SExpr] = P(boolean | number | character | string)

  def apply(input: String) = parse(input, expression(_))
}
