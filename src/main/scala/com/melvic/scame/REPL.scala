package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.Eval.EvalConfig
import com.melvic.scame.SExpr.Definition
import fastparse.Parsed
import zio.{Runtime, ZIO}
import zio.console._
import zio.internal.PlatformLive

/**
 * Implementation for Scheme's Read-Eval-Print-Loop
 */
object REPL {
  def apply() = {
    def recurse(env: Env): ZIO[Console, Any, Unit]  = for {
      _ <- putStr("> ")
      input <- getStrLn
      newEnv <- if (input == "exit") exit else parse(input, env)
      _ <- recurse(newEnv.getOrElse(env))
    } yield ()

    recurse(EmptyEnv)
  }

  def parse(input: String, env: Env) = Parse(input) match {
    case failure: Parsed.Failure =>
      putStrLn(s"Parse Error: ${failure.msg}").flatMap(_ => ZIO.succeed(None))
    case Parsed.Success(value, _) => for {
      result <- Eval.apply.provide(EvalConfig(value, env)).either
      newEnv <- result match {
        case Left(err) => error(err)
        case Right(sexpr) => success(sexpr)
      }
    } yield newEnv
  }

  def exit = putStrLn("Bye!").flatMap(_ => ZIO.fail(None))

  def error(err: ErrorCode) = for {
    _ <- putStrLn(Show[ErrorCode](err))

    // Return succeed to avoid breaking out of the REPL
    n <- ZIO.succeed(None)
  } yield n

  def success(sexpr: SExpr) = for {
    _ <- putStrLn(Show[SExpr](sexpr))
    newEnv <- ZIO.succeed(sexpr match {
      case Definition(newEnv) => Some(newEnv)
      case _ => None
    })
  } yield newEnv
}
