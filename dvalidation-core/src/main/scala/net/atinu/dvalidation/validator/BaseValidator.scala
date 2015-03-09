package net.atinu.dvalidation.validator

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import net.atinu.dvalidation.errors._

import scala.util.Try
import scalaz.{ IsEmpty, Unapply, Equal, Monoid }

trait BaseValidator extends ValidatorBase {

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
   *
   *  {{{
   *     import scalaz.std.anyVal._
   * notZero(1) === scalaz.Success(1)
   *  }}}
   *
   * @see [[IsZeroError]]
   */
  def notZero[T](s: T)(implicit m: Monoid[T], e: Equal[T], mapError: ErrorMap[IsZeroError]): DValidation[T] =
    if (m.isMZero(s)) failMapped(new IsZeroError(s)) else s.valid

  /**
   * Check if a value is empty in a generic way
   *
   * @param ma a value to be tested for emptiness
   * @tparam MA a shape supported by [[scalaz.Unapply]]
   * @see [[IsEmptyError]]
   */
  def nonEmptyGeneric[MA](ma: MA)(implicit U: Unapply[IsEmpty, MA], mapError: ErrorMap[IsEmptyError]): DValidation[MA] = {
    if (U.TC.isEmpty(U(ma))) failMapped(new IsEmptyError(ma))
    else valid(ma)
  }

  /**
   * Check if a collection has at least one element
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
   * Check if a [[Try]] is a [[scala.util.Success]]
   * @see [[IsTryFailureError]]
   */
  def isTrySuccess[T <: Try[_]](s: T)(implicit mapError: ErrorMap[IsTryFailureError]): DValidation[T] =
    s match {
      case value: scala.util.Success[_] => s.valid
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
   * Validator for an optional value
   * @return if parameter is Some(a) then a will be validated otherwise scalaz.Success(a)
   */
  def validOpt[T](a: Option[T])(v: DValidator[T]): DValidation[Option[T]] = {
    validateOptBase(a, v, None.valid)
  }

  /**
   * Validator for an non optional [[scala.Option]] value
   * @return if parameter is Some(a) then a will be validated otherwise scalaz.Failure
   */
  def validOptRequired[T](a: Option[T])(v: DValidator[T])(implicit mapError: ErrorMap[IsNoneError]): DValidation[Option[T]] = {
    validateOptBase(a, v, failMapped(new IsNoneError()))
  }

  private def validateOptBase[T](a: Option[T], v: DValidator[T], err: => DValidation[Option[T]]): DValidation[Option[T]] =
    a match {
      case Some(value) => v(value).map(Option.apply)
      case _ => err
    }

  /**
   * Validator for [[scala.util.Try]] values
   */
  def validTry[T](a: Try[T])(v: DValidator[T])(implicit mapError: ErrorMap[IsTryFailureError]): DValidation[Try[T]] = {
    a match {
      case scala.util.Success(g) => v(g).map(scala.util.Success.apply)
      case scala.util.Failure(e) => failMapped(new IsTryFailureError(e))
    }
  }

}
