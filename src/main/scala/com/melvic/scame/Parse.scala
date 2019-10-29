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
  def signedInt[_: P] = P("-".!.? ~ integer).map {
    case (None, sInt) => sInt
    case (Some(_), SInt(i)) => SInt(-i)
  }

  def rational[_: P] = P(signedInt ~ "/" ~ integer).map { case (SInt(n), SInt(d)) =>
    SRational(n, d)
  }
  def real[_: P] = P(signedInt ~ "." ~ integer).map { case (SInt(d), SInt(f)) =>
    val dec = Math.abs(d)
    val sign = Integer.signum(d)
    val frac = f.toDouble / SMath.tens(f)
    SReal((if (sign == 0) 1 else sign) * (dec + frac))
  }
  def number[_: P] = P(rational | real | signedInt)

  def specialCharacter[_: P] = P(NewLineLiteral | TabLiteral | SpaceLiteral | BackspaceLiteral)

  def character[_: P] = P(("#\\" ~ (specialCharacter | AnyChar)).!).map(SChar)

  def symbol[_: P] = P(CharsWhile(c => !invalidSymbol.contains(c.toString)).!).map(SSymbol)

  def define[_: P] = P(DefineLiteral).map(_ => Define)
  def quote[_: P] = P(QuoteLiteral).map(_ => Quote)
  def sLambda[_: P] = P(LambdaLiteral).map(_ => Lambda)
  def cond[_: P] = P(CondLiteral).map(_ => Cond)
  def let[_: P] = P(LetLiteral).map(_ => Let)

  def specialForm[_: P] = P(define | quote | sLambda | cond | let)

  def add[_: P] = P("+").map(_ => Add)
  def subtract[_: P] = P("-").map(_ => Subtract)
  def multiply[_: P] = P("*").map(_ => Multiply)
  def divide[_: P] = P("/").map(_ => Divide)

  def arithmetic[_: P] = P(add | subtract | multiply | divide)

  def function[_: P] = P(arithmetic)

  def quoteSugar[_: P] = P("'" ~ expression).map(expr => Cons(Quote, Cons(expr, SNil)))

  def atom[_: P]: P[Atom] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[SList] = P("(" ~ spaces ~ expression.rep(0, sep=" ".rep(1)) ~ spaces ~ ")").map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P(atom | sList | quoteSugar | function | symbol)

  def apply(input: String) = parse(input, expression(_))
}
