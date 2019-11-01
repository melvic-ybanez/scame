package com.melvic.scame.show

trait Show[A] extends (A => String)

object Show {
  def apply[A](value: A)(implicit show: Show[A]): String = show(value)
}
