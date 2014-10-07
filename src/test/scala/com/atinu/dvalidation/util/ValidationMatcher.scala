package com.atinu.dvalidation.util

import org.scalatest.matchers.{ MatchResult, Matcher }

import scalaz._

trait ValidationMatcher {

  class ValidationIsSuccessMatcher extends Matcher[Validation[_, _]] {
    override def apply(validation: Validation[_, _]): MatchResult = {
      MatchResult(validation.isSuccess,
        s"validation failed with errors ($validation)",
        s"validation successful")
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

  def beInvalid = new ValidationIsInvalidMatcher

}
