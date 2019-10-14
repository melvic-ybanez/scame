package com.melvic.scame

sealed trait Expr

object Expr {
  sealed trait Atom extends Expr

  case object SFalse extends Atom
  case object STrue extends Atom

  final case class Character(char: Char) extends Expr

  final case class Symbol(name: String) extends Expr

  sealed trait SList extends Expr
  case object SNil extends SList
  final case class Cons(head: Expr, tail: SList) extends SList
  final case class Pair(first: Expr, second: Expr) extends Expr

  // Special Forms
  case object Define extends Expr
  case object Lambda extends Expr
  case object Cons extends Expr
  case object Quote extends Expr

  // Special Forms Evaluated
  final case class Define(name: String, value: Expr) extends Expr
  final case class Quote(body: Expr) extends Expr
  final case class Lambda(params: Expr, body: Expr) extends Expr
  final case class Cond(pairs: Vector[Cons]) extends Expr
  final case class Let(pairs: Vector[Cons], body: Expr) extends Expr

  final case class Car(pair: Pair) extends Expr
  final case class Cdr(pair: Pair) extends Expr

  final case class Definition(env: Env) extends Expr
}