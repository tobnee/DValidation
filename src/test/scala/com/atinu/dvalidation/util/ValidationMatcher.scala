package com.atinu.dvalidation.util

import com.atinu.dvalidation.{ DomainErrors, DomainError, DValidation }
import org.scalatest.matchers.{ MatchResult, Matcher }

import scalaz._

object ValidationMatcher {
  def succ(msg: String) = MatchResult(true, "", msg)
  def fail(msg: String) = MatchResult(false, msg, "")
}

trait ValidationMatcher {

  import ValidationMatcher._

  class ValidationIsSuccessMatcher extends Matcher[Validation[_, _]] {
    override def apply(validation: Validation[_, _]): MatchResult = {
      MatchResult(validation.isSuccess,
        s"validation failed with errors ($validation)",
        s"validation successful")
    }
  }

  class ValidationValueIsSuccessMatcher[T](value: T) extends Matcher[DValidation[_]] {
    override def apply(validation: DValidation[_]): MatchResult = {
      validation match {
        case Success(v) if v == value => succ("validation successful")
        case Success(v) => fail(s"Expected $v got $value")
        case Failure(e) => fail(s"expected $value got failed validation ($e)")
      }
    }
  }

  class ValidationValueIsFailureMatcher[T <: DomainError](value: T) extends Matcher[DValidation[_]] {
    override def apply(validation: DValidation[_]): MatchResult = {
      validation match {
        case Success(v) => fail(s"expected $value got successful validation ($v)")
        case Failure(e) if e == DomainErrors(NonEmptyList(value)) => succ("validation successful")
        case Failure(e) => fail(s"Expected $e got $value")
      }
    }
  }

  class ValidationValuesIsFailureMatcher[T <: DomainError](values: Seq[T]) extends Matcher[DValidation[_]] {
    override def apply(validation: DValidation[_]): MatchResult = {
      if (values.isEmpty) fail("expected at least one expected error in test")
      else {
        val valuesList: NonEmptyList[T] = NonEmptyList.apply(values.head, values.tail: _*)
        validation match {
          case Success(v) => fail(s"expected (${values.mkString(",")}) got successful validation ($v)")
          case Failure(e) if e == DomainErrors(valuesList) => succ("validation successful")
          case Failure(e) => fail(s"Expected (${values.mkString(",")}) got $e")
        }
      }
    }
  }

  class ValidationIsInvalidMatcher extends Matcher[Validation[_, _]] {
    override def apply(validation: Validation[_, _]): MatchResult = {
      MatchResult(validation.isFailure,
        "validation successful",
        s"validation failed with errors ($validation)"
      )
    }
  }

  def beValid = new ValidationIsSuccessMatcher

  def beValidResult[T](value: T) = new ValidationValueIsSuccessMatcher(value)

  def beInvalid = new ValidationIsInvalidMatcher

  def beInvalidWithError[T <: DomainError](value: T) = new ValidationValueIsFailureMatcher(value)

  def beInvalidWithErrors[T <: DomainError](values: T*) = new ValidationValuesIsFailureMatcher(values.toSeq)

}
