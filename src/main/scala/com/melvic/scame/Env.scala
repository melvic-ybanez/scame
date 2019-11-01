package com.melvic.scame

import com.melvic.scame.ErrorCode.{SymbolAlreadyExists, SymbolNotFound}
import zio.ZIO

/**
 * This is the environment that contains the values for the bounded
 * symbols. To implement Scheme's lexical scoping, every environment
 * but the source lives inside a parent environment, which represents
 * the outer scope in a program.
 */
sealed trait Env

object Env {
  /**
   * Every environment function requires the name of the symbol
   * to query or register and the environment itself.
   */
  type EnvConfig = (String, Env)

  type EnvOp[E, A] = ZIO[EnvConfig, E, A]

  /**
   * Every read operation can either return the expression that
   * corresponds to the symbol's value or an error.
   */
  type ReadEnv = EnvOp[SymbolNotFound, SExpr]


  case object EmptyEnv extends Env

  final case class NonEmptyEnv(lookup: Map[String, SExpr], parent: Env) extends Env

  def register(expr: SExpr): EnvOp[SymbolAlreadyExists, Env] = {
    val add: ZIO[EnvConfig, SExpr, Env] = for {
      // Registration is only allowed if the identifier is not found locally.
      // So we flip the result.
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
    // update the environment to point to the parent
    case (name, NonEmptyEnv(_, parent)) => (name, parent)
    case env => env
  })
}
