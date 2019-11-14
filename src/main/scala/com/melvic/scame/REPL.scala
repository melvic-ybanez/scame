package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.Run.EvalHandler
import com.melvic.scame.SExpr.Definition
import com.melvic.scame.errors.ErrorCode
import com.melvic.scame.exprs.SExpr
import com.melvic.scame.utils.Show
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
      newEnv <- if (input == "exit") exit else Run.fromString(input, env, identity)(handleResult)
      _ <- recurse(newEnv.getOrElse(env))
    } yield ()

    recurse(EmptyEnv)
  }

  def exit = putStrLn("Bye!").flatMap(_ => ZIO.fail(None))

  def handleResult: EvalHandler[Option[Env]] = {
    case Left(err) => output(Show[ErrorCode](err))
    case Right(sexpr) => output(Show[SExpr](sexpr), sexpr match {
      case Definition(newEnv) => Some(newEnv)
      case _ => None
    })
  }

  def output(msg: String, r: => Option[Env] = None) = for {
    _ <- putStrLn(msg)
    n <- ZIO.succeed(r)
  } yield n
}
