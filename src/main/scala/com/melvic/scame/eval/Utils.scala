package com.melvic.scame.eval

import com.melvic.scame.ErrorCode.{ExprMismatch, TooFewArguments, TooManyArguments}
import com.melvic.scame.{Constants, Env, ErrorCode, SExpr}
import com.melvic.scame.SExpr._
import Utils._

trait Utils {
  /**
   * Short-circuiting fold designed specifically for evaluated s-expressions.
   * TODO: This is not tail-recursive
   */
  def foldS: FoldS = {
    case (SNil, acc) => _ => acc.!
    case (head :: tail, acc) => f => for {
      h <- Eval(head)
      a <- f(acc, h)
      r <- a match {
        case Return(e) => e.!
        case _ => foldS(tail, a)(f)
      }
    } yield r
  }

  def register(name: String, expr: SExpr) =
    Env.register(expr).provideSome[EvalConfig](e => (name, e.env))

  def boolFunc: FoldS = {
    case (args, arg) => f => for {
      acc <- Eval(arg)
      expr <- foldS(args, acc)(f)
      bool <- expr match {
        case SFalse => SFalse.!
        case _ => STrue.!
      }
    } yield bool
  }

  def binOp(op: SExpr, a: SNumber, b: SNumber) =
    Eval(op :: a :: b :: SNil)

  def argsCountOutOfBounds(
      pred: (Int, Int) => Boolean, error: (Int, Int) => ErrorCode): ArgsCountError =
    (op, count) => {
        case `op` :: args if pred(args.size, count) => error(count, args.size).!
      }

  def atLeastNArgsCountError: ArgsCountError = argsCountOutOfBounds(_ < _, TooFewArguments)
  def atMostNArgsCountError: ArgsCountError = argsCountOutOfBounds(_ > _, TooManyArguments)

  def requireArgsCount(g: ArgsCountError): ArgsCountEval = (op, count) => f =>
    g(op, count) orElse f

  def requireMinArgsCount: ArgsCountEval = requireArgsCount(atLeastNArgsCountError)

  def requireMaxArgsCount: ArgsCountEval = requireArgsCount(atMostNArgsCountError)

  def requireArgsCount: ArgsCountEval = { (op, count) => f =>
    atLeastNArgsCountError(op, count) orElse atMostNArgsCountError(op, count) orElse f
  }

  def nonNumber(expr: SExpr) = ExprMismatch(Vector(Constants.Number), expr).!

  def reverseBinOp(op: SExpr): PartialFunction[(SExpr, SExpr), Evaluation] = {
    case (f: SRational, i: SInt) => binOp(op, i, f)
    case (r: SReal, i: SInt) => binOp(op, i, r)
    case (r: SReal, f: SRational) => binOp(op, f, r)
    case (_, expr) => nonNumber(expr)
  }
}

object Utils {
  type ArgsCountError = (SExpr, Int) => PartialEval
  type ArgsCountEval = (SExpr, Int) => PartialEval => PartialEval
}
