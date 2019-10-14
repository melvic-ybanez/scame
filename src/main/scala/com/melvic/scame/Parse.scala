package com.melvic.scame

import fastparse._
import ScriptWhitespace._
import com.melvic.scame.Expr.{Character, SFalse, STrue}

object Parse {
  def boolean[_: P] = {
    def sTrue[_: P] = P("#t").map(_ => STrue)
    def sFalse[_: P] = P("#f").map(_ => SFalse)

    P(sTrue | sFalse)
  }

  def character[_: P] = {
    def specialCharacter = {
      def newline[_: P] = P("#\\newline").map(_ => Character('\n'))
      def tab[_: P] = P("#\\tab").map(_ => Character('\t'))
      def space[_: P] = P("#\\ " | "#\\space").map(_ => Character(' '))
      def backspace[_: P] = P("#\\backspace").map(_ => Character('\b'))

      P(newline | tab | space | backspace)
    }

    def regularChar[_: P] = P("#\\" ~ AnyChar.!).map(c => Character(c.head))

    P(specialCharacter | regularChar)
  }

  def apply[_: P]: P[Expr] = P(boolean | character)
}
