package com.melvic.scame

import fastparse._
import NoWhitespace._
import com.melvic.scame.SExpr.{Define, Quote, _}
import Literals._

// TODO: Add support for comments
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

  def symbol[_: P] = P(CharsWhile(c => !invalidSymbol.contains(c.toString)).!).map(SSymbol)

  def define[_: P] = P(DefineLiteral).map(_ => Define)
  def quote[_: P] = P(QuoteLiteral).map(_ => Quote)
  def sLambda[_: P] = P(LambdaLiteral).map(_ => Lambda)
  def cond[_: P] = P(CondLiteral).map(_ => Cond)
  def let[_: P] = P(LetLiteral).map(_ => Let)

  def specialForm[_: P] = P(define | quote | sLambda | cond | let)

  def quoteSugar[_: P] = P("'" ~ expression).map(expr => Cons(Quote, Cons(expr, SNil)))

  def atom[_: P]: P[Atom] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[SList] = P("(" ~ spaces ~ expression.rep(0, sep=" ".rep(1)) ~ ")").map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P(atom | sList | quoteSugar | symbol)

  def apply(input: String) = parse(input, expression(_))
}
