package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.SExpr.Definition
import fastparse.Parsed
import zio.ZIO
import zio.console._

object REPL {
  def apply() = {
    def recurse(env: Env): ZIO[Console, Any, Unit]  = for {
      _ <- putStr("> ")
      input <- getStrLn
      newEnv <- if (input == "exit") exit else parse(input)
      _ <- recurse(newEnv.getOrElse(env))
    } yield ()

    recurse(EmptyEnv)
  }

  def parse(input: String) = Parse(input) match {
    case failure: Parsed.Failure =>
      putStrLn(s"Parse Error: ${failure.msg}").flatMap(_ => ZIO.succeed(None))
    case Parsed.Success(value, _) => for {
      _ <- putStrLn(Show[SExpr](value))

      // TODO: Evaluate the value using Eval
      newEnv <- ZIO.succeed(value match {
        case Definition(env) => Some(env)
        case _ => None
      })
    } yield newEnv
  }

  def exit = putStrLn("Bye!").flatMap(_ => ZIO.fail(None))
}
