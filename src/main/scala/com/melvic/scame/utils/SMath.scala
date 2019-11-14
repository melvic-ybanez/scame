package com.melvic.scame.utils

import com.melvic.scame.exprs.SExpr.{SRational, SReal}

import scala.annotation.tailrec

object SMath {
  def lcm(a: Int, b: Int): Int =
    if (a == 0 && b == 0) 0
    else {
      val absA = a.abs
      val absB = b.abs
      val low = absA.min(absB)
      val high = absA.max(absB)

      @tailrec
      def iter(lcm: Int): Int =
        if (lcm % low == 0) lcm else iter(lcm + high)

      iter(high)
    }

  def tens(n: Int): Int = {
    @tailrec
    def recurse(t: Int, n1: Int): Int = {
      if (n1 == 0) t
      else if (n1 == 10) t + 1
      else recurse(t + 1, n1 / 10)
    }

    Math.pow(10, recurse(0, n)).toInt
  }

  def rationalToReal: SRational => SReal = { case SRational(n, d) =>
    SReal(n.toDouble / d)
  }

  def realToRational: SReal => SRational = { case SReal(n) =>
    val dec = n.toInt
    val frac = n - dec
    if (frac == 0) SRational(dec, 1)
    else {
      val fracString = frac.toString.split("\\.")(1)
      val denom = fracString.toInt
      val tens = Math.pow(10, fracString.length).toInt
      SRational(dec * tens + denom, denom)
    }
  }
}
