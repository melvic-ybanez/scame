package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import fastparse.Parsed
import zio.console._
import zio.{RIO, UIO, URIO, ZIO}

import scala.annotation.tailrec
import scala.io.StdIn

object REPL {
  // TODO: Update the env in every iteration
  def apply() = {
    def recurse(env: Env): ZIO[Console, Any, Unit]  = for {
      _ <- putStrLn("> ")
      input <- getStrLn
      _ <- if (input == "exit") exit else parse(input)
    } yield recurse(env)

    recurse(EmptyEnv)
  }

  def parse(input: String) = Parse(input) match {
    case failure: Parsed.Failure =>
      putStrLn(failure.msg)
    case Parsed.Success(value, _) =>
      putStrLn(Show[Expr](value))
  }

  def exit = {
    putStrLn("exit")
    ZIO.fail(())
  }
}
