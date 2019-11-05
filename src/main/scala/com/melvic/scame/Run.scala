package com.melvic.scame

import com.melvic.scame.Env.EmptyEnv
import com.melvic.scame.Eval.EvalConfig
import com.melvic.scame.SExpr._
import com.melvic.scame.show.Show
import fastparse.Parsed
import zio.console._
import zio.{URIO, ZIO}

import scala.io.Source

object Run {
  type EvalHandler[H] = Either[ErrorCode, SExpr] => URIO[Console, H]

  def fromString[A: Empty](input: String, env: Env, g: SExpr => SExpr)(f: EvalHandler[A]) =
    Parse(input) match {
      case failure: Parsed.Failure => for {
        _ <- putStrLn(s"Parse Error: ${failure.msg}")
        r <- ZIO.succeed(Empty[A]())
      } yield r

      case Parsed.Success(value, _) => for {
        evaluation <- Eval.apply.provide(EvalConfig(g(value), env)).either
        result <- f(evaluation)
      } yield result
    }

  def fromFile(fileName: String) = for {
    sourceCode <- ZIO.succeed {
      val source = Source.fromFile(fileName)
      try source.mkString finally source.close()
    }
    result <- fromString(sourceCode, EmptyEnv, expr => Quote :: expr :: SNil) {
      case Left(err) => putStrLn(Show[ErrorCode](err))
      case _ => ZIO.succeed(())
    }
  } yield result

  trait Empty[A] extends (() => A)

  object Empty {
    def apply[A]()(implicit empty: Empty[A]): A = empty.apply()

    implicit val emptyUnit: Empty[Unit] = () => ()
    implicit val emptyOption: Empty[Option[Env]] = () => None
  }
}
