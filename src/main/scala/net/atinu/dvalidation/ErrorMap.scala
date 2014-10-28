package net.atinu.dvalidation

trait ErrorMap[-T] extends (T => DomainError)

object ErrorMap {

  private val id: PartialFunction[DomainError, DomainError] = { case e => e }

  def apply[T <: DomainError](f: T => DomainError): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = f(error)
  }

  def dispatch(f: PartialFunction[DomainError, DomainError]): ErrorMap[DomainError] = new ErrorMap[DomainError] {
    def apply(error: DomainError) = f.orElse(id)(error)
  }

  def mapKey[T <: DomainError](key: String): ErrorMap[T] = new ErrorMap[T] {
    def apply(error: T) = CustomValidationError.withKey(error, key)
  }

  implicit object DomainErrorIdentity extends ErrorMap[DomainError] {
    def apply(in: DomainError): DomainError = in
  }
}