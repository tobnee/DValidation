package net.atinu.dvalidation

import net.atinu.dvalidation.errors.CustomValidationError
import scala.language.implicitConversions

/**
 * Enables a user of a validator to map generic error value(s) to domain specific ones
 * @tparam T a [[DomainError]]
 */
trait ErrorMap[-T <: DomainError] extends (T => DomainError)

object ErrorMap {

  private val id: PartialFunction[DomainError, DomainError] = { case e => e }

  def apply[T <: DomainError](f: T => DomainError): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = f(error)
  }

  /**
   * Build [[ErrorMap]] for all [[DomainError]] values. If no mapping is done a fallback
   * to the input value is provided
   */
  def dispatch(f: PartialFunction[DomainError, DomainError]): ErrorMap[DomainError] =
    dispatchFor[DomainError](f)

  /**
   * Build [[ErrorMap]] for specified [[DomainError]] values. If no mapping is done a fallback
   * to the input value is provided
   */
  def dispatchFor[T <: DomainError](f: PartialFunction[T, DomainError]): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T): DomainError = f.orElse(id)(error)
  }

  /**
   * Maps the content of a [[DomainError]] to a [[CustomValidationError]] with the specified key
   */
  def mapKey[T <: DomainError](key: String): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = CustomValidationError.withKey(error, key)
  }

  implicit object DomainErrorIdentity extends ErrorMap[DomainError] {
    def apply(in: DomainError): DomainError = in
  }

  implicit def function1ToErrorMap[T <: DomainError](f: T => DomainError): ErrorMap[T] = ErrorMap.apply(f)
}