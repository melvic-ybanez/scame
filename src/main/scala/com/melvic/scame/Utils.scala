package com.melvic.scame

import com.melvic.scame.SExpr.{SRational, SReal}

import scala.annotation.tailrec

object Utils {
  def lcm(a: Int, b: Int): Int =
    if (a == 0 && b == 0) 0
    else {
      val absA = Math.abs(a)
      val absB = Math.abs(b)
      val low = Math.min(absA, absB)
      val high = Math.max(absA, absB)

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
}
