package com.melvic.scame.exprs

import com.melvic.scame.Env

sealed trait SExpr

// TODO: Strings, Vectors, Conversion between types
//  see: https://ds26gte.github.io/tyscheme/index-Z-H-4.html
object SExpr {
  sealed trait Atom extends SExpr
  sealed trait SFunction extends Atom
  sealed trait SpecialForm extends Atom

  sealed trait SBoolean extends Atom
  case object SFalse extends SBoolean
  case object STrue extends SBoolean

  final case class SChar(value: String) extends Atom

  final case class SSymbol(name: String) extends SExpr

  // TODO: Add support for complex numbers, and other base format (binary, hex, etc.)
  // TODO: Add support for castings
  sealed trait SNumber extends Atom
  final case class SInt(value: Int) extends SNumber
  final case class SRational(numerator: Int, denominator: Int) extends SNumber
  final case class SReal(value: Double) extends SNumber

  sealed trait SList extends SExpr {
    def ::(sexpr: SExpr): SList = SExpr.::(sexpr, this)
  }
  case object SNil extends SList
  case object SList extends SFunction
  final case class ::(head: SExpr, tail: SList) extends SList
  final case class Pair(first: SExpr, second: SExpr) extends SExpr

  // Special Forms as heads of the lists
  case object Define extends SpecialForm
  case object Lambda extends SpecialForm
  case object Quote extends SpecialForm
  case object Cond extends SpecialForm    // TODO: Support for If
  case object Let extends SpecialForm     // TODO: Support for LetRec

  // Special Forms
  final case class Define(name: String, value: SExpr) extends SExpr
  final case class Lambda(params: SExpr, body: SExpr) extends SExpr

  final case class Car(pair: Pair) extends SExpr
  final case class Cdr(pair: Pair) extends SExpr

  final case class Definition(env: Env) extends SExpr

  // Arithmetic operators
  sealed trait Arithmetic extends SFunction
  case object Add extends Arithmetic
  case object Subtract extends Arithmetic
  case object Multiply extends Arithmetic
  case object Divide extends Arithmetic

  // Relational operators
  sealed trait Relational extends SFunction
  case object EqSign extends Relational
  case object GT extends Relational
  case object GTE extends Relational
  case object LT extends Relational
  case object LTE extends Relational

  case object Eq extends SFunction
  case object Equal extends SFunction
  case object Cons extends SFunction
  case object Null extends SFunction
  case object Car extends SFunction
  case object Cdr extends SFunction

  final case class Return(expr: SExpr) extends SExpr

  def falsy: SExpr => Boolean = {
    case SFalse => true
    case _ => false
  }
}