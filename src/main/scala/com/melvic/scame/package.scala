package com.melvic

package object scame extends Implicits with ShowInstances {
  type ErrorOr[A] = Either[ErrorCode, A]
}
