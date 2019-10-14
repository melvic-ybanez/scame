package com.melvic.scame

import fastparse.Parsed

import scala.annotation.tailrec
import scala.io.StdIn

object REPL {
  @tailrec
  def apply(env: Env): Unit = {
    val input = StdIn.readLine("> ")
    if (input == "exit") println("Bye!")
    else Parse(input) match {
      case failure: Parsed.Failure =>
        println(failure.msg)
        REPL(env)
      case Parsed.Success(value, _) =>
        println(Show[Expr](value))
        REPL(env)
    }
  }
}
