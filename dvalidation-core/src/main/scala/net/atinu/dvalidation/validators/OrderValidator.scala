package net.atinu.dvalidation.validators

import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.{ IsNotLowerThenError, IsNotGreaterThenError }

import scalaz.{ Failure, Order, Semigroup }

object OrderValidator extends OrderValidator

trait OrderValidator extends ValidatorBase {

  /**
   * Checks a > b or a >= b
   * @param isInclusive change to >= (default >)
   * @see [[net.atinu.dvalidation.errors.IsNotGreaterThenError]]
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

}
