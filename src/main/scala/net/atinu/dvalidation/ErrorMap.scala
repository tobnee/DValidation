package net.atinu.dvalidation

import net.atinu.dvalidation.errors.CustomValidationError

trait ErrorMap[-T] extends (T => DomainError)

object ErrorMap {

  private val id: PartialFunction[DomainError, DomainError] = { case e => e }

  def apply[T <: DomainError](f: T => DomainError): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = f(error)
  }

  def dispatch(f: PartialFunction[DomainError, DomainError]): ErrorMap[DomainError] =
    dispatchFor[DomainError](f)

  def dispatchFor[T <: DomainError](f: PartialFunction[T, DomainError]): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T): DomainError = f.orElse(id)(error)
  }

  def mapKey[T <: DomainError](key: String): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = CustomValidationError.withKey(error, key)
  }

  implicit object DomainErrorIdentity extends ErrorMap[DomainError] {
    def apply(in: DomainError): DomainError = in
  }
}