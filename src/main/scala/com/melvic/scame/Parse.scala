package com.melvic.scame

import com.melvic.scame.Literals._
import com.melvic.scame.errors.IndexedNode
import com.melvic.scame.exprs.SExpr
import com.melvic.scame.exprs.SExpr._
import com.melvic.scame.utils.SMath
import fastparse.NoWhitespace._
import fastparse._

object Parse {
  type Indexed[A] = (Int, A)
  type IndexedParser[A] = P[Indexed[A]]

  /** Indexed Parser */
  def IP[_: P, A](parser: => P[A]): IndexedParser[A] = P(Index ~ parser)

  def ip[A](f: A => SExpr)(p: Indexed[A]) = p._1 -> f(p._2)

  implicit class IPOps[A](indexedParer: IndexedParser[A]) {
    def mapIp(f: A => SExpr) = indexedParer.map(ip(f))
  }

  def spaces[_: P] = P(CharsWhileIn(" \r\n", 0))

  def literal[_: P, E <: SExpr](kv: (String, E)) = kv match {
    case (pattern, expr) => IP(pattern).mapIp(_ => expr)
  }

  def literals[_: P, E <: SExpr](literalsMap: Map[String, E]) =
    literalsMap.tail.foldLeft(literal(literalsMap.head)) {
      case (parser, (key, value)) => parser | literal((key, value))
    }

  def boolean[_: P] = literals(Map(TrueLiteral -> STrue, FalseLiteral -> SFalse))

  def integer[_: P] = IP(CharsWhileIn("0-9").!).mapIp(i => SInt(i.toInt))

  def signedInt[_: P] = IP("-".!.? ~ integer).mapIp {
    // Ignore the inner index. We only care about the whole (negated) integer.
    case (None, (_, sInt)) => sInt
    case (Some(_), (_, SInt(i))) => SInt(-i)
  }

  def rational[_: P] = IP(signedInt ~ "/" ~ integer).mapIp {
    case (_, SInt(n), (_, SInt(d))) => SRational(n, d)
  }

  def real[_: P] = IP(signedInt ~ "." ~ integer).mapIp { case (_, SInt(d), (_, SInt(f))) =>
    val dec = Math.abs(d)
    val sign = Integer.signum(d)
    val frac = f.toDouble / SMath.tens(f)
    SReal((if (sign == 0) 1 else sign) * (dec + frac))
  }
  def number[_: P] = P(rational | real | signedInt)

  def specialCharacter[_: P] = P(NewLineLiteral | TabLiteral | SpaceLiteral | BackspaceLiteral)

  def character[_: P] = IP(("#\\" ~ (specialCharacter | AnyChar)).!).mapIp(SChar)

  def symbol[_: P] = IP(CharsWhile(c => !invalidSymbol.contains(c.toString)).!).mapIp(SSymbol)

  def specialForm[_: P] = literals(Map(DefineLiteral -> Define,
    QuoteLiteral -> Quote, LambdaLiteral -> Lambda, CondLiteral -> Cond, LetLiteral -> Let))

  def function[_: P] = P(literals(FunctionMap))

  def quoteSugar[_: P] = P("'" ~ AnyChar.!).flatMap(expr => expression(s"(quote $expr)"))

  def atom[_: P] = P(boolean | number | specialForm | character)

  def sList[_: P]: P[IndexedNode] = IP("(" ~ spaces ~ expression.rep(0, sep=" ".rep(1)) ~ spaces ~ ")")
    // Register nodes to the stack trace
    .map { case (index, indexedExprs) =>
      val (stackChildren, sList) = indexedExprs.foldLeft[(List[IndexedNode], SList)]((Nil, SNil)) {
        case ((children, acc), in @ IndexedNode(sexpr, _, _)) => (in :: children, sexpr :: acc)
      }
      IndexedNode(sList, index, stackChildren)
    }

  // TODO: Add support for comments
  def expression[_: P]: P[IndexedNode] = P(atom | sList | quoteSugar | function | symbol).map {
    case (pos: Int, expr: SExpr) => IndexedNode(expr, pos)
    case a: IndexedNode => a
  }

  def apply(input: String) = parse(input, expression(_))
}
