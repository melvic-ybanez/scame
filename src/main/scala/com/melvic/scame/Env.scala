package com.melvic.scame

import com.melvic.scame.ErrorCode.SymbolAlreadyExists

final case class Env(lookup: Map[String, Expr], parent: Env) {
  def apply(name: String): Option[Expr] = localSearch(name).orElse(parent(name))

  def localSearch(name: String): Option[Expr] = lookup.get(name)

  def +(name: String, expr: Expr): ErrorOr[Env] =
    localSearch(name).map(_ => SymbolAlreadyExists(name)).toLeft(this)
}