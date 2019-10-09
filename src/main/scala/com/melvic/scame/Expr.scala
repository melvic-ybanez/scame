package com.melvic.scame

sealed trait Expr

object Expr {
  final case class Identifier(name: String) extends Expr

  sealed trait SList extends Expr
  case object SNil extends SList
  final case class Pair(first: Expr, second: Expr) extends SList

  sealed trait SForm extends Expr
  final case class Define(name: String, body: Expr) extends SForm
  final case class Quote(body: Expr) extends SForm
  final case class Lambda(param: Identifier, body: Expr) extends SForm
  final case class Cond(pairs: Vector[SList]) extends SForm
  final case class Let(pairs: Vector[SList], body: Expr) extends SForm

  sealed trait Func extends Expr
  final case class Car(pair: Pair) extends Func
  final case class Cdr(pair: Pair) extends Func
}