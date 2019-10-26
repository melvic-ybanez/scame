package com.melvic.scame

sealed trait SExpr

// TODO: Strings, Vectors, Procedure, Conversion between types
//  see: https://ds26gte.github.io/tyscheme/index-Z-H-4.html
object SExpr {
  sealed trait Atom extends SExpr

  sealed trait SBoolean extends Atom
  case object SFalse extends SBoolean
  case object STrue extends SBoolean

  final case class SChar(value: String) extends Atom

  final case class SSymbol(name: String) extends SExpr

  // TODO: Add support for complex numbers, and other base format (binary, hex, etc.)
  // TODO: Add support for negative numbers
  sealed trait SNumber extends Atom
  final case class SInt(value: Int) extends SNumber
  final case class SRational(numerator: Int, denominator: Int) extends SNumber
  final case class SReal(whole: Int, fractional: Int) extends SNumber

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

  // Special Forms
  final case class Define(name: String, value: SExpr) extends SExpr
  final case class Quote(body: SExpr) extends SExpr
  final case class Lambda(params: SExpr, body: SExpr) extends SExpr
  final case class Cond(pairs: Vector[Cons]) extends SExpr
  final case class Let(pairs: Vector[Cons], body: SExpr) extends SExpr

  final case class Car(pair: Pair) extends SExpr
  final case class Cdr(pair: Pair) extends SExpr

  final case class Definition(env: Env) extends SExpr
}