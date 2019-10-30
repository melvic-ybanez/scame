package com.melvic.scame

sealed trait SExpr

// TODO: Strings, Vectors, Procedure, Conversion between types
//  see: https://ds26gte.github.io/tyscheme/index-Z-H-4.html
object SExpr {
  sealed trait Atom extends SExpr

  sealed trait SBoolean extends Atom

  object SBoolean {
    def apply(bool: Boolean): SBoolean =
      if (bool) STrue else SFalse
  }

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

  // Lists
  sealed trait SList extends SExpr
  case object SNil extends SList
  final case class Cons(head: SExpr, tail: SList) extends SList
  final case class Pair(first: SExpr, second: SExpr) extends SExpr

  // Special Forms as heads of the lists
  sealed trait SpecialForm extends Atom
  case object Define extends SpecialForm
  case object Lambda extends SpecialForm
  case object Cons extends SpecialForm
  case object Quote extends SpecialForm
  case object Cond extends SpecialForm
  case object Let extends SpecialForm

  // Special Forms
  final case class Define(name: String, value: SExpr) extends SExpr
  final case class Lambda(params: SExpr, body: SExpr) extends SExpr

  final case class Car(pair: Pair) extends SExpr
  final case class Cdr(pair: Pair) extends SExpr

  final case class Definition(env: Env) extends SExpr

  // Arithmetic operators
  sealed trait Arithmetic extends SExpr
  case object Add extends Arithmetic
  case object Subtract extends Arithmetic
  case object Multiply extends Arithmetic
  case object Divide extends Arithmetic

  // Relational operators
  sealed trait Relational extends SExpr
  case object Equal extends Relational
  case object GT extends Relational
  case object GTE extends Relational
  case object LT extends Relational
  case object LTE extends Relational

  def falsy: SExpr => Boolean = {
    case SFalse => true
    case _ => false
  }
}