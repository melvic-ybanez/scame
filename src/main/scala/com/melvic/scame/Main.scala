package com.melvic.scame

import zio._

object Main extends App {
  override def run(args: List[String]) = REPL().fold(_ => 1, _ => 0)
}
