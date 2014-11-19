package net.atinu.dvalidation

import net.atinu.dvalidation.errors.{ CustomValidationError, IsNotEqualError, IsNotLowerThenError, IsNotGreaterThenError }
import net.atinu.dvalidation.validator.{ BaseValidator, OrderValidator, SizedValidator }

import scalaz.{ Equal, Order }
import scalaz.syntax.validation._

object Validator extends BaseValidator with OrderValidator with SizedValidator {

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
  def ensure[T](s: T)(key: String, args: Any*)(v: T => Boolean)(implicit mapError: ErrorMap[CustomValidationError]): DValidation[T] =
    if (v(s)) s.success else failMapped(new CustomValidationError(s, key, args.map(_.toString)))

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

  /**
   * Create a validation error for a given value
   * @param key see [[DomainError.msgKey]]
   */
  def invalid[T](value: Any, key: String) = new CustomValidationError(value, key).invalid[T]

  /**
   * Create a validation error for a given value
   * @param key see [[DomainError.msgKey]]
   * @param args see [[DomainError.args]]
   */
  def invalid[T](value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq).invalid[T]

  /**
   * Lift a value to a valid validation
   */
  def valid[T](value: T): DValidation[T] = value.valid

  implicit class ValidationCombinatorSyntax[T](val a: T) extends AnyVal {
    def is_>(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotGreaterThenError]) = isGreaterThan(a, b)
    def is_>=(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotGreaterThenError]) = isGreaterThan(a, b, isInclusive = true)
    def is_<(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotLowerThenError]) = isSmallerThan(a, b)
    def is_<=(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotLowerThenError]) = isSmallerThan(a, b, isInclusive = true)
    def is_==(b: T)(implicit mapError: ErrorMap[IsNotEqualError]) = isEqual(a, b)
    def is_===(b: T)(implicit ev: Equal[T], mapError: ErrorMap[IsNotEqualError]) = isEqualStrict(a, b)
  }

}

