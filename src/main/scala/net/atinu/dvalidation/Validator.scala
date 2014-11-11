package net.atinu.dvalidation

import scala.util.Try
import scalaz._
import scalaz.syntax.validation._

object Validator {

  /**
   * Check if a string is not blank
   * @param trimWhitespace remove the leading and trailing whitespace before checking
   * @see [[IsEmptyStringError]]
   */
  def notBlank(s: String, trimWhitespace: Boolean = true)(implicit mapError: ErrorMap[IsEmptyStringError]): DValidation[String] = {
    val testStr = if (trimWhitespace) s.trim else s
    if (testStr.isEmpty) failMapped(new IsEmptyStringError()) else s.valid
  }

  /**
   * Check if a value is a [[Monoid.zero]]
   * @see [[IsZeroError]]
   */
  def notZero[T](s: T)(implicit m: Monoid[T], e: Equal[T], mapError: ErrorMap[IsZeroError]): DValidation[T] =
    if (m.isMZero(s)) failMapped(new IsZeroError(s)) else s.success

  /**
   * Check if a collections has at least one element
   * @see [[IsEmptySeqError]]
   */
  def hasElements[T <: Traversable[_]](s: T)(implicit mapError: ErrorMap[IsEmptySeqError]): DValidation[T] =
    if (s.isEmpty) failMapped(new IsEmptySeqError()) else s.valid

  /**
   * Check if an [[Option]] is a [[Some]]
   * @see [[IsNoneError]]
   */
  def isSome[T <: Option[_]](s: T)(implicit mapError: ErrorMap[IsNoneError]): DValidation[T] =
    if (s.isEmpty) failMapped(new IsNoneError()) else s.valid

  /**
   * Check if a [[Try]] is a [[Success]]
   * @see [[IsTryFailureError]]
   */
  def isTrySuccess[T <: Try[_]](s: T)(implicit mapError: ErrorMap[IsTryFailureError]): DValidation[T] =
    s match {
      case value: scala.util.Success[_] => s.success
      case scala.util.Failure(e) => failMapped(new IsTryFailureError(e))
    }

  /**
   * Uses the == operator on AnyRef for checking equality
   * @see [[IsNotEqualError]]
   */
  def isEqual[T](value: T, valueExpected: T)(implicit mapError: ErrorMap[IsNotEqualError]): DValidation[T] =
    if (value == valueExpected) value.valid
    else failMapped(new IsNotEqualError(value, valueExpected))

  /**
   * Uses an instance of [[scalaz.Equal]] for checking equality
   * @see [[IsNotEqualError]]
   */
  def isEqualStrict[T](value: T, valueExpected: T)(implicit ev: Equal[T], mapError: ErrorMap[IsNotEqualError]): DValidation[T] =
    if (ev.equal(value, valueExpected)) value.valid
    else failMapped(new IsNotEqualError(value, valueExpected))

  /**
   * Checks a > b or a >= b
   * @param isInclusive change to >= (default >)
   * @see [[IsNotGreaterThenError]]
   */
  def isGreaterThan[T](value: T, valueMin: T, isInclusive: Boolean = false)(implicit ev: Order[T], mapError: ErrorMap[IsNotGreaterThenError]): DValidation[T] = {
    val isGt = if (isInclusive) ev.greaterThanOrEqual _ else ev.greaterThan _
    if (isGt(value, valueMin)) value.valid
    else failMapped(new IsNotGreaterThenError(valueMin, value, isInclusive))
  }

  /**
   * Checks a < b or a <= b
   * @param isInclusive change to <= (default <)
   * @see [[IsNotGreaterThenError]]
   */
  def isSmallerThan[T](value: T, valueMax: T, isInclusive: Boolean = false)(implicit ev: Order[T], mapError: ErrorMap[IsNotLowerThenError]): DValidation[T] = {
    val isLt = if (isInclusive) ev.lessThanOrEqual _ else ev.lessThanOrEqual _
    if (isLt(value, valueMax)) value.valid
    else failMapped(new IsNotLowerThenError(valueMax, value, isInclusive))
  }

  /**
   * Validate if a value is in a range of min < value < max
   * @see [[IsNotGreaterThenError]]
   * @see [[IsNotLowerThenError]]
   */
  def isInRange[T](value: T, min: T, max: T, inclusiveMin: Boolean = false, inclusiveMax: Boolean = false)(implicit ev: Order[T], mapError: ErrorMap[DomainError]): DValidation[T] = {
    if (ev.greaterThanOrEqual(min, max)) throw new IllegalArgumentException(s"wrong validation definition min: $min >= max: $max")
    else accumulateErrors(isSmallerThan(value, max, inclusiveMin), isGreaterThan(value, min, inclusiveMax))
  }

  private def accumulateErrors[EE, AA](t: DValidation[AA], that: DValidation[AA])(implicit es: Semigroup[DomainErrors]): DValidation[AA] = t match {
    case Failure(e) => that match {
      case Failure(e0) => Failure(es.append(e, e0))
      case success => t
    }
    case success => that
  }

  def hasSize[T](value: T, min: Int = Int.MinValue, max: Int = Int.MaxValue)(implicit f: Sized[T], mapError: ErrorMap[WrongSizeError]) = {
    if (max < min) throw new IllegalArgumentException(s"wrong validation definition min: $min >= max: $max")
    val size = f.size(value)
    if (size <= max) {
      if (size < min) failMapped(new IsToSmallError(min, size))
      else valid(value)
    } else failMapped(new IsToBigError(max, size))
  }

  private def failMapped[A, T <: DomainError](err: T)(implicit me: ErrorMap[T]): DValidation[A] = me(err).invalid

  implicit class ValidationCombinatorSyntax[T](val a: T) extends AnyVal {
    def is_>(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotGreaterThenError]) = isGreaterThan(a, b)
    def is_>=(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotGreaterThenError]) = isGreaterThan(a, b, isInclusive = true)
    def is_<(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotLowerThenError]) = isSmallerThan(a, b)
    def is_<=(b: T)(implicit ev: Order[T], mapError: ErrorMap[IsNotLowerThenError]) = isSmallerThan(a, b, isInclusive = true)
    def is_==(b: T)(implicit mapError: ErrorMap[IsNotEqualError]) = isEqual(a, b)
    def is_===(b: T)(implicit ev: Equal[T], mapError: ErrorMap[IsNotEqualError]) = isEqualStrict(a, b)
  }

  trait Sized[T] {
    def size(v: T): Int
  }

  implicit def FoldableAsSized[A, T[A]](implicit f: Foldable[T]): Sized[T[A]] = new Sized[T[A]] {
    def size(v: T[A]): Int = f.length(v)
  }

  implicit object StringAsSized extends Sized[String] {
    def size(v: String): Int = v.length
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

  def validOpt[T](a: Option[T])(v: DValidator[T]): DValidation[Option[T]] = {
    validateOptBase(a, v, valid(None))
  }

  def validOptRequired[T](a: Option[T])(v: DValidator[T])(implicit mapError: ErrorMap[IsNoneError]): DValidation[Option[T]] = {
    validateOptBase(a, v, failMapped(new IsNoneError()))
  }

  private def validateOptBase[T](a: Option[T], v: DValidator[T], err: => DValidation[Option[T]]): DValidation[Option[T]] =
    a match {
      case Some(value) => v(value).map(Option.apply)
      case _ => err
    }

  def validTry[T](a: Try[T])(v: DValidator[T])(implicit mapError: ErrorMap[IsTryFailureError]): DValidation[Try[T]] = {
    a match {
      case scala.util.Success(g) => v(g).map(scala.util.Success.apply)
      case scala.util.Failure(e) => failMapped(new IsTryFailureError(e))
    }
  }

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

