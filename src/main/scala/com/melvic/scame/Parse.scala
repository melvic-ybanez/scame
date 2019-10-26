package com.melvic.scame

import fastparse._
import NoWhitespace._
import com.melvic.scame.SExpr.{Define, Quote, _}
import Literals._

object Parse {
  def spaces[_: P] = P(CharsWhileIn(" \r\n", 0))

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
    CharsWhile(c => !invalidSymbol.contains(c.toString)).?).!).map(SSymbol)

  def string[_: P] = P("\"" ~ CharsWhile(_ != '\"').?.! ~ "\"").map {
    // Strings are just lists of characters, at least for now.
    // Note that this is subject to change as the goal is to get
    // closer to the standard scheme language design.
    _.toList.map(c => SChar(s"#\\$c")).asSList
  }

  def define[_: P] = P(DefineLiteral).map(_ => Define)
  def quote[_: P] = P(QuoteLiteral).map(_ => Quote)
  def sLambda[_: P] = P(LambdaLiteral).map(_ => Lambda)

  def specialForm[_: P] = P(define | quote | sLambda)

  def quoteSugar[_: P] = P("'" ~ expression).map(expr => Cons(Quote, Cons(expr, SNil)))

  def atom[_: P]: P[Atom] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[SList] = P("(" ~ spaces ~ expression.rep(0, sep=spaces) ~ spaces ~ ")").map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P(spaces ~ (atom | sList | symbol | string | quoteSugar) ~ spaces)

  def apply(input: String) = parse(input, expression(_))
}
