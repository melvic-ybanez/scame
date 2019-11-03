package com.melvic.scame

import com.melvic.scame.Literals._
import com.melvic.scame.SExpr.{Define, Quote, _}
import com.melvic.scame.SExpr.SList._
import fastparse.NoWhitespace._
import fastparse._

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

  def literal[_: P, E <: SExpr](kv: (String, E)) = P(kv._1).map(_ => kv._2)

  def literals[_: P, E <: SExpr](literalsMap: Map[String, E]) =
    literalsMap.tail.foldLeft(literal(literalsMap.head)) {
      case (parser, kv) => P(parser | literal(kv))
    }

  def specialForm[_: P] = literals(Map(DefineLiteral -> Define,
    QuoteLiteral -> Quote, LambdaLiteral -> Lambda, CondLiteral -> Cond, LetLiteral -> Let))

  def arithmetic[_: P] = literals(ArithmeticMap)

  def relational[_: P] = literals(RelationalMap)

  def function[_: P] = P(arithmetic | relational)

  def quoteSugar[_: P] = P("'" ~ expression).map(expr => ::(Quote, ::(expr, SNil)))

  def atom[_: P]: P[Atom] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[SList] = P("(" ~ spaces ~ expression.rep(0, sep=" ".rep(1)) ~ spaces ~ ")")
    .map(_.toList.asSList)

  def expression[_: P]: P[SExpr] = P(atom | sList | quoteSugar | function | symbol)

  def apply(input: String) = parse(input, expression(_))
}
