package com.melvic.scame.utils

trait Show[A] extends (A => String)

object Show {
  def apply[A](value: A)(implicit show: Show[A]): String = show(value)
}
