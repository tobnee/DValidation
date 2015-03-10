package net.atinu.dvalidation

import net.atinu.dvalidation.Path._

package object errors {

  /**
   * A base class which can be used to define a custom [[DomainError]]
   */
  abstract class AbstractDomainError(valueP: Any, msgKeyP: String, pathP: PathString, argsP: Seq[String] = Nil) extends DomainError {

    def value = valueP

    def msgKey = msgKeyP

    def path = pathP

    def args = argsP

    def nest(path: PathString): DomainError = {
      copyWithPath(this.path.nest(path))
    }

    def nestIndex(index: Int): DomainError = {
      copyWithPath(path.nestIndex(index))
    }

    def nestAttribute(segment: Symbol): DomainError = {
      copyWithPath(path.nestSymbol(segment))
    }

    private def argsString = if (args.isEmpty) "" else s", args: ${args.mkString(",")}"

    override def toString = s"""DomainError(path: $path, value: $value, msgKey: $msgKey$argsString)"""

    override def equals(value: Any) = value match {
      case v: AbstractDomainError if v.getClass == this.getClass =>
        v.value == this.value &&
          v.msgKey == this.msgKey &&
          v.path == this.path &&
          v.args == this.args
      case _ => false
    }

    override def hashCode(): Int =
      41 * (
        41 * (
          41 * (
            41 + value.hashCode
          ) + msgKey.hashCode
        ) + Path.unwrap(path).hashCode
      ) + args.hashCode
  }

  class IsNotEqualError(valueExpected: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.notEqual", path) with DomainErrorWithExpectation {
    def copyWithPath(path: PathString) = new IsNotEqualError(valueExpected, value, path)

    def expected = valueExpected

    override def args = Seq(expected.toString)
  }

  class IsEqualToError(value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.isEqual", path) {
    def copyWithPath(path: PathString) = new IsEqualToError(value, path)
  }

  class IsEmptyError(v: Any, path: PathString = Path./) extends AbstractDomainError(v, "error.dvalidation.isEmpty", path) {
    def copyWithPath(path: PathString) = new IsEmptyError(v, path)
  }

  class IsEmptyStringError(path: PathString = Path./) extends AbstractDomainError("", "error.dvalidation.emptyString", path) {
    def copyWithPath(path: PathString) = new IsEmptyStringError(path)
  }

  class IsZeroError(value: Any, path: PathString = Path./) extends AbstractDomainError(value, "error.dvalidation.notEqual", path) {
    def copyWithPath(path: PathString) = new IsZeroError(value, path)
  }

  class IsEmptySeqError(path: PathString = Path./) extends AbstractDomainError(Nil, "error.dvalidation.emptySeq", path) {

    def copyWithPath(path: PathString) = new IsEmptySeqError(path)
  }

  class IsNoneError(path: PathString = Path./) extends AbstractDomainError(None, "error.dvalidation.isNone", path) {

    def copyWithPath(path: PathString) = new IsNoneError(path)
  }

  class IsTryFailureError(value: Throwable, path: PathString = Path./) extends AbstractDomainError(value, "error.dvalidation.isTryFailue", path) {

    def copyWithPath(path: PathString) = new IsTryFailureError(value, path)
  }

  class IsNotGreaterThenError(valueMin: Any, value: Any, val isInclusive: Boolean, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.notGreaterThen", path)
      with DomainErrorWithExpectation {

    def copyWithPath(path: PathString) = new IsNotGreaterThenError(valueMin, value, isInclusive, path)

    def expected = valueMin

    override def args = Seq(expected.toString, isInclusive.toString)
  }

  class IsNotLowerThenError(valueMax: Any, value: Any, val isInclusive: Boolean, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.notSmallerThen", path)
      with DomainErrorWithExpectation {

    def copyWithPath(path: PathString) = new IsNotLowerThenError(valueMax, value, isInclusive, path)

    def expected = valueMax

    override def args = Seq(valueMax.toString, isInclusive.toString)
  }

  sealed trait WrongSizeError extends DomainErrorWithExpectation

  class IsToSmallError(valueMin: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.tooSmallError", path) with WrongSizeError {

    def copyWithPath(path: PathString) = new IsToSmallError(valueMin, value, path)

    def expected = valueMin

    override def args = Seq(expected.toString)
  }

  class IsToBigError(valueMax: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.tooBigError", path) with WrongSizeError {

    def copyWithPath(path: PathString) = new IsToBigError(valueMax, value, path)

    def expected = valueMax

    override def args = Seq(expected.toString)
  }

  object CustomValidationError {
    def apply(value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq)

    def withKey(e: DomainError, msgKey: String) = new CustomValidationError(e.value, msgKey, e.args, e.path)
  }

  class CustomValidationError(value: Any, key: String, args: Seq[String] = Nil, path: PathString = Path./) extends AbstractDomainError(value, key, path, args) {

    def copyWithPath(path: PathString) = new CustomValidationError(value, key, args, path)
  }

}
