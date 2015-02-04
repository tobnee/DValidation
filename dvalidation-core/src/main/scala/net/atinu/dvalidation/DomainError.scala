package net.atinu.dvalidation

import net.atinu.dvalidation.Path.PathString

import scalaz.{ Show, Equal, Semigroup }

object DomainError {

  def unapply(v: DomainError): Some[(Any, String, PathString, Seq[String])] =
    Some((v.value, v.msgKey, v.path, v.args))

  implicit class ErrorToFailure(val error: DomainError) extends AnyVal {
    import scalaz.syntax.validation._
    /**
     * lift id [[DomainError]] to id failed [[DValidation]]
     */
    def invalid[T]: DValidation[T] = DomainErrors.withSingleError(error).failure
  }

  implicit def domainErrorInstances =
    new Equal[DomainError] with Show[DomainError] {
      override def shows(f: DomainError) = f.toString

      def equal(a1: DomainError, a2: DomainError): Boolean = a1 == a2
    }
}

/**
 * Represents an error and attaches information to it which can be used for
 * translation and error handling in general.
 */
trait DomainError {
  /** reference value of the error */
  def value: Any

  /** a string representation of the error type (e.g. dvalidaton.errors.foo) */
  def msgKey: String

  /** a path to map an error to its location in a nested object */
  def path: PathString

  /** arguments of the error (e.g. expected values) */
  def args: Seq[String]

  /**
   * create a new domain error with a new path, replacing old path information
   */
  def copyWithPath(path: PathString): DomainError

  /**
   * create a new domain error with a path prepended to the existing path
   * (e.g. starting with a path /e and an argument /a the result is /e/a)
   */
  def nest(path: PathString): DomainError

  /**
   * @param segment a path represented as symbol
   * @return a domain error with a new path prepended with the name of
   *         the symbol
   * @see [[nest]]
   */
  def nestAttribute(segment: Symbol): DomainError

  /**
   * @param index a path represented as int
   * @return a domain error with a new path prepended with the index
   *         surrounded by [] (e.g from /a with 1 to /a/[1])
   * @see [[nest]]
   */
  def nestIndex(index: Int): DomainError
}

