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

  def specialCharacter[_: P] = P(NewLineLiteral | TabLiteral | SpaceLiteral | BackspaceLiteral)

  def character[_: P] = P(("#\\" ~ (specialCharacter | AnyChar)).!).map(SChar)

  /**
   * Matches any name that starts with an underscore, dollar sign,
   * or alpha-character.
   * Note: This is subject to change in the future.
   */
  def symbol[_: P] = P((("_" | "$" | CharIn("a-zA-Z")) ~
    CharsWhile(c => invalidSymbol.contains(c.toString)).rep).!).map(SSymbol)

  def string[_: P] = P("\"" ~~ CharsWhile(_ != '\"').! ~~ "\"").map { expr =>
    // Strings are just lists of characters, at least for now.
    // Note that this is subject to change as the goal is to get
    // closer to the standard scheme language design.
    val str = expr.toList.map(c => SChar(c.toString)).asSList
    Cons(Quote, str)
  }

  def define[_: P] = P(DefineLiteral).map(_ => Define)
  def quote[_: P] = P(QuoteLiteral).map(_ => Quote)
  def sLambda[_: P] = P(LambdaLiteral).map(_ => Lambda)

  def specialForm[_: P] = P(define | quote | sLambda)

  def atom[_: P]: P[Atom] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[SList] = P("(" ~/ expression.repX(0, sep=" ".repX(0)) ~ ")").map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P((atom | symbol | sList | string) ~ End)

  def apply(input: String) = parse(input, expression(_))
}
