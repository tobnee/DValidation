package net.atinu.dvalidation

import scala.util.Try
import scalaz._
import scalaz.syntax.validation._

object Validator {

  /**
   * @see [[IsEmptyStringError]]
   */
  def notBlank(s: String, trimWhitespace: Boolean = true): DValidation[String] = {
    val testStr = if (trimWhitespace) s.trim else s
    if (testStr.isEmpty) new IsEmptyStringError().invalid else s.valid
  }

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
   * Uses the == operator on AnyRef for checking equality
   * @see [[IsNotEqualError]]
   */
  def isEqual[T](value: T, valueExpected: T): DValidation[T] =
    if (value == valueExpected) value.valid
    else new IsNotEqualError(value, valueExpected).invalid

  /**
   * Uses an instance of [[scalaz.Equal]] for checking equality
   * @see [[IsNotEqualError]]
   */
  def isEqualStrict[T](value: T, valueExpected: T)(implicit ev: Equal[T]): DValidation[T] =
    if (ev.equal(value, valueExpected)) value.valid
    else new IsNotEqualError(value, valueExpected).invalid

  /**
   * @see [[IsNotGreaterThenError]]
   */
  def isGreaterThan[T](value: T, valueMin: T, isInclusive: Boolean = false)(implicit ev: Order[T]): DValidation[T] = {
    val isGt = if (isInclusive) ev.greaterThanOrEqual _ else ev.greaterThan _
    if (isGt(value, valueMin)) value.valid
    else new IsNotGreaterThenError(valueMin, value, isInclusive).invalid
  }

  /**
   * @see [[IsNotGreaterThenError]]
   */
  def isSmallerThan[T](value: T, valueMax: T, isInclusive: Boolean = false)(implicit ev: Order[T]): DValidation[T] = {
    val isLt = if (isInclusive) ev.lessThanOrEqual _ else ev.lessThanOrEqual _
    if (isLt(value, valueMax)) value.valid
    else new IsNotLowerThenError(valueMax, value, isInclusive).invalid
  }

  /**
   * Validate if a value is in a range of min < value < max
   * @see [[IsNotGreaterThenError]]
   * @see [[IsNotGreaterThenError]]
   */
  def isInRange[T](value: T, min: T, max: T, inclusiveMin: Boolean = false, inclusiveMax: Boolean = false)(implicit ev: Order[T]): DValidation[T] = {
    if (ev.greaterThanOrEqual(min, max)) throw new IllegalArgumentException(s"wrong validation definition min: $min >= max: $max")
    else accumulateErrors(isSmallerThan(value, max, inclusiveMin), isGreaterThan(value, min, inclusiveMax))
  }

  private def accumulateErrors[EE, AA](t: Validation[EE, AA], that: Validation[EE, AA])(implicit es: Semigroup[EE]): Validation[EE, AA] = t match {
    case Failure(e) => that match {
      case Failure(e0) => Failure(es.append(e, e0))
      case success => t
    }
    case success => that
  }

  implicit class ValidationCombinatorSyntax[T](val a: T) extends AnyVal {
    def is_>(b: T)(implicit ev: Order[T]) = isGreaterThan(a, b)
    def is_>=(b: T)(implicit ev: Order[T]) = isGreaterThan(a, b, isInclusive = true)
    def is_<(b: T)(implicit ev: Order[T]) = isSmallerThan(a, b)
    def is_<=(b: T)(implicit ev: Order[T]) = isSmallerThan(a, b, isInclusive = true)
    def is_==(b: T) = isEqual(a, b)
    def is_===(b: T)(implicit ev: Equal[T]) = isEqualStrict(a, b)
  }

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

