package com.melvic.scame

import com.melvic.scame.ErrorCode.{SymbolAlreadyExists, SymbolNotFound}
import zio.ZIO

sealed trait Env

object Env {
  type EnvConfig = (String, Env)
  type ReadEnv = ZIO[EnvConfig, SymbolNotFound, Expr]

  case object EmptyEnv extends Env

  final case class NonEmptyEnv(lookup: Map[String, Expr], parent: Env) extends Env

  def register(expr: Expr): ZIO[EnvConfig, SymbolAlreadyExists, Env] = {
    val add: ZIO[EnvConfig, Expr, Env] = for {
      _ <- localSearch.flip
      env <- ZIO.fromFunction[EnvConfig, Env] {
        case (name, EmptyEnv) => NonEmptyEnv(Map(name -> expr), EmptyEnv)
        case (name, e @ NonEmptyEnv(lookup, _)) => e.copy(lookup = lookup + (name -> expr))
      }
    } yield env

    add.orElse {
      for {
        env <- ZIO.environment[EnvConfig]
        err <- ZIO.fromEither(Left(SymbolAlreadyExists(env._1)))
      } yield err
    }
  }

  def localSearch: ReadEnv = ZIO.fromFunctionM {
    case (name, EmptyEnv) => ZIO.fail(SymbolNotFound(name))
    case (name, NonEmptyEnv(lookup, _)) =>
      ZIO.fromEither(lookup.get(name).toRight(SymbolNotFound(name)))
  }

  def globalSearch: ReadEnv = for {
    _ <- localSearch
    env <- ZIO.access[EnvConfig] {
      case (name, NonEmptyEnv(_, parent)) => (name, parent)
    }
    global <- localSearch.provide(env)
  } yield global
}
