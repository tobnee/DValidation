package net.atinu.dvalidation.errors

import java.time.LocalDate

import net.atinu.dvalidation.Path._
import net.atinu.dvalidation.{ DomainError, Path }

object NotInPastError {
  val key = "error.dvalidation.dateInPast"
}

trait NotInPastError extends DomainError {
  def maxInPast: LocalDate
  def minInPast: LocalDate
  def time: LocalDate
}

class DefaultNotInPastError(value: Any, val maxInPast: LocalDate, val minInPast: LocalDate, val isInclusive: Boolean, path: PathString = Path./)
    extends AbstractDomainError(value, NotInPastError.key, path, Seq(isInclusive.toString)) with NotInPastError {

  def copyWithPath(path: PathString): DomainError =
    new DefaultNotInPastError(value, maxInPast, minInPast, isInclusive, path)

  override def time: LocalDate = value.asInstanceOf[LocalDate]
}

class DelegateNotInPastError(lte: IsNotLowerThenError)
    extends ErrorDelegation(lte, NotInPastError.key) with NotInPastError {

  def delegate(lte: DomainError) =
    new DelegateNotInPastError(lte.asInstanceOf[IsNotLowerThenError])

  override def maxInPast: LocalDate = ???

  override def minInPast: LocalDate = ???

  override def time: LocalDate = ???
}

object NotInFutureError {
  val key = "error.dvalidation.dateInFuture"
}

trait NotInFutureError extends DomainError

class DefaultNotInFutureError(value: Any, val isInclusive: Boolean, path: PathString = Path./)
    extends AbstractDomainError(value, NotInFutureError.key, path) {

  def copyWithPath(path: PathString): DomainError =
    new DefaultNotInPastError(value, isInclusive, path)
}

class DelegateNotInFutureError(gte: IsNotGreaterThenError)
    extends ErrorDelegation(gte, NotInFutureError.key) with NotInFutureError {

  def delegate(lte: DomainError) =
    new DelegateNotInFutureError(lte.asInstanceOf[IsNotGreaterThenError])

}
