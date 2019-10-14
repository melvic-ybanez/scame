package com.melvic.scame

import fastparse._
import ScriptWhitespace._
import com.melvic.scame.Expr.{SChar, SFalse, STrue}

object Parse {
  def boolean[_: P] = {
    def sTrue[_: P] = P(Constants.TrueLiteral).map(_ => STrue)
    def sFalse[_: P] = P(Constants.FalseLiteral).map(_ => SFalse)

    P(sTrue | sFalse)
  }

  def character[_: P] = {
    def specialCharacter = {
      def newline[_: P] = P("#\\newline").map(_ => SChar("\n"))
      def tab[_: P] = P("#\\tab").map(_ => SChar("\t"))
      def space[_: P] = P("#\\ " | "#\\space").map(_ => SChar(" "))
      def backspace[_: P] = P("#\\backspace").map(_ => SChar("\b"))

      P(newline | tab | space | backspace)
    }

    def regularChar[_: P] = P("#\\" ~ AnyChar.!).map(SChar)

    P(specialCharacter | regularChar)
  }

  def expression[_: P]: P[Expr] = P(boolean | character)

  def apply(input: String) = parse(input, expression(_))
}
