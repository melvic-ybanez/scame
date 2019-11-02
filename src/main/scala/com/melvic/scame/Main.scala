package com.melvic.scame

import zio._

object Main extends App {
  override def run(args: List[String]) = (args match {
    case Nil => REPL()
    case fileName :: Nil => Run.fromFile(fileName)
    case args => ZIO.fail(s"Too many arguments. Required: At most 1. Got ${args.length}")
  }).fold(_ => 1, _ => 0)
}
