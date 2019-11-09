package com.melvic.scame

import com.melvic.scame.SExpr.SList
import zio.ZIO

package object eval extends Utils {
  type FoldS = (SList, SExpr) => ((SExpr, SExpr) => Evaluation) => Evaluation
  type EvaluationE[E] = ZIO[EvalConfig, ErrorCode, E]
  type Evaluation = EvaluationE[SExpr]
  type PartialEval = PartialFunction[SExpr, Evaluation]

  final case class EvalConfig(expr: SExpr, env: Env)
}
