package net.atinu.dvalidation.util

import net.atinu.dvalidation.Path._
import net.atinu.dvalidation.{ Path, DomainErrors, DomainError, DValidation }
import org.scalatest.matchers.{ MatchResult, Matcher }

import scalaz._

object ValidationMatcher {
  def succ(msg: String) = MatchResult(true, "", msg)
  def fail(msg: String) = MatchResult(false, msg, "")

  def matchValidations(validation: DValidation[_], expect: NonEmptyList[DomainError]): MatchResult = {
    def errorDisplay = expect.list.toList.mkString(",")
    validation match {
      case Success(v) => fail(s"expected ($errorDisplay) got successful validation ($v)")
      case Failure(e) if e == DomainErrors.fromNel(expect) => succ("validation successful")
      case Failure(e) => fail(s"Expected ($errorDisplay) got $e")
    }
  }
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
      val valuesList: NonEmptyList[DomainError] = NonEmptyList.apply(value)
      matchValidations(validation, valuesList)
    }
  }

  class ValidationValuesIsFailureMatcher[T <: DomainError](values: Seq[T]) extends Matcher[DValidation[_]] {
    override def apply(validation: DValidation[_]): MatchResult = {
      if (values.isEmpty) fail("expected at least one expected error in test")
      else {
        val valuesList: NonEmptyList[DomainError] = NonEmptyList.apply(values.head, values.tail: _*)
        matchValidations(validation, valuesList)
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

  class ValidationHasInvalidProperties(value: Any, key: String, path: PathString, args: Seq[String]) extends Matcher[DValidation[_]] {
    def failed(validation: DValidation[_]) = fail(s"expected domain error with properties ($value, $key, $path, $args) got $validation")

    def apply(validation: DValidation[_]): MatchResult = {
      validation match {
        case Failure(e) =>
          e match {
            case DomainErrors(e1, ex @ _*) if e1.value == value && e1.msgKey == key && e1.path == path && e1.args == args =>
              succ("")
            case _ => failed(validation)
          }
        case _ => failed(validation)
      }
    }
  }

  def beValid = new ValidationIsSuccessMatcher

  def beValidResult[T](value: T) = new ValidationValueIsSuccessMatcher(value)

  def beInvalid = new ValidationIsInvalidMatcher

  def beInvalidWithError[T <: DomainError](value: T) = new ValidationValueIsFailureMatcher(value)

  def beInvalidWithErrorProps(value: Any, key: String, path: String, args: String*) =
    new ValidationHasInvalidProperties(value, key, Path.wrap(path), args.toSeq)

  def beInvalidWithErrors[T <: DomainError](values: T*) = new ValidationValuesIsFailureMatcher(values.toSeq)

}
