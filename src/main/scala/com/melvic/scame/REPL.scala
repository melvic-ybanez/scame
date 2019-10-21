package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.Eval.EvalConfig
import com.melvic.scame.SExpr.Definition
import fastparse.Parsed
import zio.{Runtime, ZIO}
import zio.console._
import zio.internal.PlatformLive

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
    case Parsed.Success(value, _) =>
      val runtime: Runtime[EvalConfig] = Runtime(EvalConfig(value, env), PlatformLive.Default)
      val result = runtime.unsafeRun(Eval.evaluator)

      for {
      _ <- putStrLn(Show[SExpr](value))
      newEnv <- ZIO.succeed(result match {
        case Definition(env) => Some(env)
        case _ => None
      })
    } yield newEnv
  }

  def exit = putStrLn("Bye!").flatMap(_ => ZIO.fail(None))
}
