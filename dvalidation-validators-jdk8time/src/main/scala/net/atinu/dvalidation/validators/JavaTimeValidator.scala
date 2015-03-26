package net.atinu.dvalidation.validators

import java.time.{ Period, LocalDateTime, LocalTime, LocalDate }

import net.atinu.dvalidation.ErrorMap
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.errors._

import scalaz.Ordering

object JavaTimeValidator extends JavaTimeValidator

class JavaTimeValidator extends ValidatorBase {

  private val orderLocalDate = scalaz.Order.order[LocalDate]((a, b) =>
    if (a.isBefore(b)) Ordering.LT else Ordering.GT)

  private val orderLocalTime = scalaz.Order.order[LocalTime]((a, b) =>
    if (a.isBefore(b)) Ordering.LT else Ordering.GT)

  private val orderLocalDateTime = scalaz.Order.order[LocalDateTime]((a, b) =>
    if (a.isBefore(b)) Ordering.LT else Ordering.GT)

  def inPast(date: LocalDate, atLeast: Period = Period.ZERO, atMost: Period = Period.ZERO)(implicit mapError: ErrorMap[NotInPastError]) = {
    def em = ErrorMap.apply[IsNotLowerThenError](error => mapError(new DelegateNotInPastError(error)))
    isSmallerThan(date, LocalDate.now().minus(atLeast), isInclusive = false)(orderLocalDate, em)
  }

  def inFuture(date: LocalDate, atLeast: Period = Period.ZERO, atMost: Period = Period.ZERO)(implicit mapError: ErrorMap[NotInFutureError]) = {
    def em = ErrorMap.apply[IsNotGreaterThenError](error => mapError(new DelegateNotInFutureError(error)))
    isGreaterThan(date, LocalDate.now().plus(atLeast), isInclusive = false)(orderLocalDate, em)
  }

  //  def inPast(date: LocalTime)(implicit mapError: ErrorMap[NotInPastError]) = {
  //    def em = ErrorMap.apply[IsNotLowerThenError](error => mapError(new DelegateNotInPastError(error)))
  //    isSmallerThan(date, LocalTime.now(), isInclusive = false)(orderLocalTime, em)
  //  }
  //
  //  def inFuture(date: LocalTime)(implicit mapError: ErrorMap[NotInFutureError]) = {
  //    def em = ErrorMap.apply[IsNotGreaterThenError](error => mapError(new DelegateNotInFutureError(error)))
  //    isGreaterThan(date, LocalTime.now(), isInclusive = false)(orderLocalTime, em)
  //  }
  //
  //  def inPast(date: LocalDateTime)(implicit mapError: ErrorMap[NotInPastError]) = {
  //    def em = ErrorMap.apply[IsNotLowerThenError](error => mapError(new DelegateNotInPastError(error)))
  //    isSmallerThan(date, LocalDateTime.now(), isInclusive = false)(orderLocalDateTime, em)
  //  }
  //
  //  def inFuture(date: LocalDateTime)(implicit mapError: ErrorMap[NotInFutureError]) = {
  //    def em = ErrorMap.apply[IsNotGreaterThenError](error => mapError(new DelegateNotInFutureError(error)))
  //    isGreaterThan(date, LocalDateTime.now(), isInclusive = false)(orderLocalDateTime, em)
  //  }
}
