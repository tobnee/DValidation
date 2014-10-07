package com.atinu.dvalidation

import scalaz._
import scalaz.NonEmptyList._
import scalaz.syntax.validation._

object Validator {

  import DomainErrors._

  def notEmpty(s: String): DValidation[String] =
    if (s.isEmpty) new IsEmptyStringError(s).invalid else s.valid

  def hasElements[T <: Traversable[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsEmptySeqError(s).invalid else s.valid

  def hasElement[T <: Option[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsNoneError(s).invalid else s.valid

  def ensure[T](s: T)(errorStr: String, v: T => Boolean): DValidation[T] =
    if (v(s)) s.success else new CustomValidationError(s, errorStr).invalid

}

object DomainErrors {

  type DValidation[T] = Validation[DomainErrors, T]

  def invalid[T](value: Any, key: String) = new CustomValidationError(value, key).invalid[T]

  def valid[T](value: T): DValidation[T] = value.valid

  implicit class ErrorToErrors(val error: DomainError) extends AnyVal {
    def invalid[T] = new DomainErrors(NonEmptyList.apply(error)).fail[T]
  }

  implicit class tToSuccess[T](val value: T) extends AnyVal {
    def valid: DValidation[T] = value.success[DomainErrors]
  }

  implicit class tToValidation[T](val value: T) extends AnyVal {
    import syntax.semigroup._
    def validateWith(validations: DValidation[_]*): DValidation[T] = {
      val validValue = valid(value)
      validations.foldLeft(validValue) {
        case (Success(_), Success(_)) => validValue
        case (Success(_), e @ Failure(_)) => e.asInstanceOf[DValidation[T]]
        case (Failure(e1), Failure(e2)) => (e1 |+| e2).fail
        case (e @Failure(_), Success(_)) => e.asInstanceOf[DValidation[T]]
      }
    }
  }

  implicit class dValFirstSuccess[T](val value: DValidation[T]) extends AnyVal {
    def isValidOr[R <: T](next: => DValidation[R]) = value.findSuccess(next)
    def forAttribute(attr: String): DValidation[T] = {
      value.leftMap(domainErrors => domainErrors.copy(errors =
        domainErrors.errors.map(e => e.nestPath(attr))))
    }
  }

  implicit def errorsSemiGroup: Semigroup[DomainErrors] =
    new Semigroup[DomainErrors] {
      def append(f1: DomainErrors, f2: => DomainErrors): DomainErrors = {
        val errors = Semigroup[NonEmptyList[DomainError]].append(f1.errors, f2.errors)
        new DomainErrors(errors)
      }
    }
}

case class DomainErrors(errors: NonEmptyList[DomainError]) {
  override def toString = errors.list.mkString(",")
  def prettyPrint = errors.list.mkString("-->\n", "\n", "\n<--")
}

trait DomainError {
  def value: Any
  def msgKey: String
  def path: String
  def nestPath(segment: String): DomainError = {
    val newPath =
      if(path == "/") s"/$segment"
      else s"/$segment$path"
    copyWithPath(newPath)
  }
  def copyWithPath(path: String): DomainError
}

abstract class AbstractDomainError(valueP: Any, msgKeyP: String, pathP: String = "/") extends DomainError {
  def value = valueP
  def msgKey = msgKeyP
  def path = pathP

  override def toString = s"""DomainError(path: $path, value: $value, msgKey: $msgKey)"""
}

class IsEmptyStringError(value: String, path: String = "/") extends AbstractDomainError(value, "error.dvalidation.emptyString", path) {
   def copyWithPath(path: String): IsEmptyStringError = new IsEmptyStringError(value, path)
}

class IsEmptySeqError(value: Traversable[_], path: String = "/") extends AbstractDomainError(value, "error.dvalidation.emptySeq", path) {

   def copyWithPath(path: String): IsEmptySeqError = new IsEmptySeqError(value, path)
}

class IsNoneError(value: Option[_], path: String = "/") extends AbstractDomainError(value, "error.dvalidation.isNone", path) {

   def copyWithPath(path: String): IsNoneError = new IsNoneError(value, path)
}

class CustomValidationError(value: Any, key: String, path: String = "/") extends AbstractDomainError(value, key, path) {

   def copyWithPath(path: String): CustomValidationError = new CustomValidationError(value, key, path)
}

