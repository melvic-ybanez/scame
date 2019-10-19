package com.melvic.scame

import fastparse._
import ScriptWhitespace._
import com.melvic.scame.Expr.{SChar, SFalse, STrue}
import Literals._

object Parse {
  def sTrue[_: P] = P(TrueLiteral).map(_ => STrue)
  def sFalse[_: P] = P(FalseLiteral).map(_ => SFalse)
  def boolean[_: P] = P(sTrue | sFalse)

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

  def expression[_: P]: P[Expr] = P(boolean | character)

  def apply(input: String) = parse(input, expression(_))
}
