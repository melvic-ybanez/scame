package com.melvic.scame

import com.melvic.scame.ErrorCode.{SymbolAlreadyExists, SymbolNotFound}
import zio.ZIO

sealed trait Env

object Env {
  type EnvConfig = (String, Env)
  type ReadEnv = ZIO[EnvConfig, SymbolNotFound, SExpr]

  case object EmptyEnv extends Env

  final case class NonEmptyEnv(lookup: Map[String, SExpr], parent: Env) extends Env

  def register(expr: SExpr): ZIO[EnvConfig, SymbolAlreadyExists, Env] = {
    val add: ZIO[EnvConfig, SExpr, Env] = for {
      _ <- localSearch.flip
      env <- ZIO.access[EnvConfig] {
        case (name, EmptyEnv) => NonEmptyEnv(Map(name -> expr), EmptyEnv)
        case (name, e @ NonEmptyEnv(lookup, _)) => e.copy(lookup = lookup + (name -> expr))
      }
    } yield env

    add.orElse(ZIO.accessM[EnvConfig](e => ZIO.fail(SymbolAlreadyExists(e._1))))
  }

  def localSearch: ReadEnv = ZIO.accessM {
    case (name, EmptyEnv) => ZIO.fail(SymbolNotFound(name))
    case (name, NonEmptyEnv(lookup, _)) =>
      ZIO.fromEither(lookup.get(name).toRight(SymbolNotFound(name)))
  }

  def globalSearch: ReadEnv = localSearch.orElse(localSearch.provideSome[EnvConfig] {
    case (name, NonEmptyEnv(_, parent)) => (name, parent)
  })
}
