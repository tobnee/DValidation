package net.atinu

import scala.util.Try
import scalaz._
import scalaz.syntax.validation._

package object dvalidation {

  type DValidation[T] = Validation[DomainErrors, T]
  type DValidator[T] = T => DValidation[T]

  implicit class ErrorToFailure(val error: DomainError) extends AnyVal {
    def invalid[T] = DomainErrors.withSingleError(error).fail[T]
  }

  implicit class tToSuccess[T](val value: T) extends AnyVal {
    def valid: DValidation[T] = value.success[DomainErrors]
  }

  implicit class tToValidation[T](val value: T) extends AnyVal {
    def validateWith(validations: DValidation[_]*): DValidation[T] = {
      applyValidations(validations, value)
    }
  }

  implicit class tryToValidation[T](val value: Try[T]) extends AnyVal {
    def asValidation: DValidation[T] = Validator.isTrySuccess(value).map(_.get)
  }

  implicit class optToValidation[T](val value: Option[T]) extends AnyVal {
    def asValidation: DValidation[T] = Validator.isSome(value).map(_.get)
  }

  implicit class dvalidationToValidationNel[T](val value: DValidation[T]) extends AnyVal {
    def asValidationNel: ValidationNel[DomainError, T] = value.leftMap(_.errors)
  }

  implicit class dSeqValidation[T](val value: IndexedSeq[DValidation[T]]) extends AnyVal {

    def forAttribute(attr: Symbol): IndexedSeq[DValidation[T]] = {
      value.map(validation => nestPathOnError(validation, _.nestAttribute(attr)))
    }

    def collapse = {
      val valid = value.flatMap(v => v.toOption).valid
      validateAll(value, valid)
    }
  }

  implicit class dValFirstSuccess[T](val value: DValidation[T]) extends AnyVal {

    def forAttribute(attr: Symbol): DValidation[T] = {
      nestPathOnError(value, _.nestAttribute(attr))
    }

    def errorView = value.fold(Option.apply, _ => None)

    def withValidations(validations: Seq[DValidation[_]]) =
      validateAll(validations, value)
  }

  private[dvalidation] def applyValidations[T](validations: Seq[DValidation[_]], value: T): DValidation[T] = {
    val validValue = Validator.valid(value)
    validateAll(validations, validValue)
  }

  private[dvalidation] def nestPathOnError[T](value: DValidation[T], nestPathTransform: DomainError => DomainError) = {
    value.leftMap(domainErrors => domainErrors.map(error => nestPathTransform(error)))
  }

  private[dvalidation] def validateAll[T](validations: Seq[DValidation[_]], validValue: DValidation[T]): DValidation[T] = {

    def failed(e: Failure[DomainErrors, _]): DValidation[T] =
      e.asInstanceOf[DValidation[T]]

    validations.foldLeft(validValue) {
      case (Success(_), Success(_)) => validValue
      case (Success(_), e @ Failure(_)) => failed(e)
      case (Failure(e1), Failure(e2)) => (e1 append e2).fail
      case (e @ Failure(_), Success(_)) => failed(e)
    }
  }

  implicit def errorsSemiGroup: Semigroup[DomainErrors] =
    new Semigroup[DomainErrors] {
      def append(f1: DomainErrors, f2: => DomainErrors): DomainErrors = {
        val errors = Semigroup[NonEmptyList[DomainError]].append(f1.errors, f2.errors)
        DomainErrors.fromNel(errors)
      }
    }
}
