package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.Eval.EvalConfig
import com.melvic.scame.SExpr.Definition
import fastparse.Parsed
import zio.ZIO
import zio.console._

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
    case failure: Parsed.Failure => output(s"Parse Error: ${failure.msg}")
    case Parsed.Success(value, _) => for {
      result <- Eval.apply.provide(EvalConfig(value, env)).either
      newEnv <- result match {
        case Left(err) => output(Show[ErrorCode](err))
        case Right(sexpr) => output(Show[SExpr](sexpr), sexpr match {
          case Definition(newEnv) => Some(newEnv)
          case _ => None
        })
      }
    } yield newEnv
  }

  def exit = putStrLn("Bye!").flatMap(_ => ZIO.fail(None))

  def output(msg: String, r: => Option[Env] = None) = for {
    _ <- putStrLn(msg)
    n <- ZIO.succeed(r)
  } yield n
}
