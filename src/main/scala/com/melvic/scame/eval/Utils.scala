package com.melvic.scame.eval

import com.melvic.scame.ErrorCode.{ExprMismatch, TooFewArguments}
import com.melvic.scame.{Constants, Env, SExpr}
import com.melvic.scame.SExpr._

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

  def nonBinaryOpError(op: SExpr): PartialEval = {
    case `op` :: SNil => TooFewArguments(2, 0).!
    case `op` :: _ :: SNil => TooFewArguments(2, 1).!
  }

  def nonNumber(expr: SExpr) = ExprMismatch(Vector(Constants.Number), expr).!

  def reverseBinOp(op: SExpr): PartialFunction[(SExpr, SExpr), Evaluation] = {
    case (f: SRational, i: SInt) => binOp(op, i, f)
    case (r: SReal, i: SInt) => binOp(op, i, r)
    case (r: SReal, f: SRational) => binOp(op, f, r)
    case (_, expr) => nonNumber(expr)
  }
}
