package net.atinu.dvalidation

import scala.util.Try
import scalaz._
import scalaz.syntax.validation._

object Validator {

  def notEmpty(s: String): DValidation[String] =
    if (s.isEmpty) new IsEmptyStringError().invalid else s.valid

  def hasElements[T <: Traversable[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsEmptySeqError().invalid else s.valid

  def isSome[T <: Option[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsNoneError().invalid else s.valid

  def isTrySuccess[T <: Try[_]](s: T): DValidation[T] =
    s match {
      case value: scala.util.Success[_] => s.success
      case scala.util.Failure(e) => new IsTryFailureError(e).invalid
    }

  def isEqual[T](value: T, valueExpected: T): DValidation[T] =
    if (value == valueExpected) value.valid
    else new IsNotEqualError(value, valueExpected).invalid

  def ensure[T](s: T)(key: String, args: Any*)(v: T => Boolean): DValidation[T] =
    if (v(s)) s.success else new CustomValidationError(s, key, args.map(_.toString)).invalid

  def validSequence[T](seq: Traversable[T], validator: DValidator[T]): IndexedSeq[DValidation[T]] = {
    seq.toIndexedSeq.zipWithIndex.map {
      case (value, idx) =>
        nestPathOnError(validator(value), _.nestIndex(idx))
    }
  }

  def validate[T](value: T)(cond: T => Boolean)(error: => DomainError): DValidation[T] =
    if (cond(value)) value.valid else error.invalid

  def template[T](v: DValidator[T]): DValidator[T] = v

  def invalid[T](value: Any, key: String) = new CustomValidationError(value, key).invalid[T]

  def invalid[T](value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq).invalid[T]

  def valid[T](value: T): DValidation[T] = value.valid

}

