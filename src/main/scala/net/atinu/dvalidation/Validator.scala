package net.atinu.dvalidation

import scala.util.Try
import scalaz._
import scalaz.syntax.validation._

object Validator {

  /**
   * @see [[IsEmptyStringError]]
   */
  def notEmpty(s: String): DValidation[String] =
    if (s.isEmpty) new IsEmptyStringError().invalid else s.valid

  /**
   * @see [[IsEmptySeqError]]
   */
  def hasElements[T <: Traversable[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsEmptySeqError().invalid else s.valid

  /**
   * @see [[IsNoneError]]
   */
  def isSome[T <: Option[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsNoneError().invalid else s.valid

  /**
   * @see [[IsTryFailureError]]
   */
  def isTrySuccess[T <: Try[_]](s: T): DValidation[T] =
    s match {
      case value: scala.util.Success[_] => s.success
      case scala.util.Failure(e) => new IsTryFailureError(e).invalid
    }

  /**
   * @see [[IsNotEqualError]]
   */
  def isEqual[T](value: T, valueExpected: T): DValidation[T] =
    if (value == valueExpected) value.valid
    else new IsNotEqualError(value, valueExpected).invalid

  /**
   * A ad-hoc validation function
   *
   * {{{
   *    def isEqual[T](valueCheck: T, valueExcept: T): DValidation[T] =
   *     ensure(valueCheck)("error.dvalidation.isequal", valueExcept)(a => a == valueExcept)
   * }}}
   *
   * @param s a value to be validated
   * @param key a string representation of this error
   * @param args parameters for the key
   * @param v predicate which indicates if the value is valid
   * @see [[CustomValidationError]]
   */
  def ensure[T](s: T)(key: String, args: Any*)(v: T => Boolean): DValidation[T] =
    if (v(s)) s.success else new CustomValidationError(s, key, args.map(_.toString)).invalid

  /**
   * Validate all elements of a given sequence
   * @param seq sequence to be validated
   * @param validator validator for the element type
   * @tparam T the element type
   * @return a sequence containing a [[DValidation]] for each input element
   */
  def validSequence[T](seq: Traversable[T], validator: DValidator[T]): IndexedSeq[DValidation[T]] = {
    seq.toIndexedSeq.zipWithIndex.map {
      case (value, idx) =>
        nestPathOnError(validator(value), _.nestIndex(idx))
    }
  }

  /**
   * Validate a value with the possibility to specify an error type
   *
   * {{{
   *    Validator.validate("a")(_ == "a")(error = new CustomValidationError("a", "error.notA"))
   * }}}
   * @param value value to be validated
   * @param cond validation predicate
   * @param error function to create a error
   */
  def validate[T](value: T)(cond: T => Boolean)(error: => DomainError): DValidation[T] =
    if (cond(value)) value.valid else error.invalid

  /**
   * Define a reusable [[DValidator]] function
   * {{{
   *  val res: DValidation[Musician] = mikael.validateWith(
   *    notEmpty(mikael.name) forAttribute 'name,
   *    ensure(mikael.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
   *    hasElements(mikael.instruments) forAttribute 'instruments
   * )
   * }}}
   */
  def template[T](v: DValidator[T]): DValidator[T] = v

  def invalid[T](value: Any, key: String) = new CustomValidationError(value, key).invalid[T]

  def invalid[T](value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq).invalid[T]

  def valid[T](value: T): DValidation[T] = value.valid

}

