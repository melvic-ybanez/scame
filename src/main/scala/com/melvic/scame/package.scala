package com.melvic

package object scame extends Implicits {
  type ErrorOr[A] = Either[ErrorCode, A]
}
