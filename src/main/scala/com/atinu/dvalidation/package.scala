package com.atinu

import scalaz.Validation

package object dvalidation {

  type DValidation[T] = Validation[DomainErrors, T]
  type DValidator[T] = T => DValidation[T]

}
