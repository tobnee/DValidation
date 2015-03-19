package net.atinu.dvalidation

import net.atinu.dvalidation.Path._

package object errors {

  /**
   * A base class which can be used to define a custom [[DomainError]]
   */
  abstract class AbstractDomainError(valueP: Any, msgKeyP: String, pathP: PathString, params: Map[String, Any] = Map.empty)
      extends DomainError with DomainErrorBase with NestingDomainError {

    def value = valueP

    def msgKey = msgKeyP

    def path = pathP

    def parameters = params

    def args = params.values.map(_.toString).toVector

  }

  trait NestingDomainError { this: DomainError =>

    def nest(path: PathString): DomainError = {
      copyWithPath(this.path.nest(path))
    }

    def nestIndex(index: Int): DomainError = {
      copyWithPath(path.nestIndex(index))
    }

    def nestAttribute(segment: Symbol): DomainError = {
      copyWithPath(path.nestSymbol(segment))
    }
  }

  trait DomainErrorBase { this: DomainError =>

    private def argsString = if (args.isEmpty) "" else s", args: ${args.mkString(",")}"

    override def toString = s"""DomainError(path: $path, value: $value, msgKey: $msgKey$argsString)"""

    override def equals(value: Any) = value match {
      case v: DomainError =>
        v.value == this.value &&
          v.msgKey == this.msgKey &&
          v.path == this.path &&
          v.parameters == this.parameters
      case _ => false
    }

    override def hashCode(): Int =
      41 * (
        41 * (
          41 * (
            41 + value.hashCode
          ) + msgKey.hashCode
        ) + Path.unwrap(path).hashCode
      ) + parameters.hashCode
  }

  abstract class ErrorDelegation(private val error: DomainError, val msgKey: String)
      extends DomainError with NestingDomainError with DomainErrorBase {

    protected def delegate(de: DomainError): DomainError

    def value: Any = error.value

    def copyWithPath(path: PathString): DomainError =
      delegate(error.copyWithPath(path))

    def args: Seq[String] = error.args

    def path: PathString = error.path

    def parameters: Map[String, Any] = error.parameters
  }

  class DefaultKeyDelegator(error: DomainError, msgKey: String) extends ErrorDelegation(error, msgKey) {

    protected def delegate(de: DomainError): DomainError =
      new DefaultKeyDelegator(de, msgKey)
  }

  class IsNotEqualError(valueExpected: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.notEqual", path) with DomainErrorWithExpectation {
    def copyWithPath(path: PathString) = new IsNotEqualError(valueExpected, value, path)

    def expected = valueExpected

    override def parameters = Map("expected" -> expected)

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

    override def parameters = Map("expected" -> expected, "isInclusive" -> isInclusive)
  }

  class IsNotLowerThenError(valueMax: Any, value: Any, val isInclusive: Boolean, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.notSmallerThen", path)
      with DomainErrorWithExpectation {

    def copyWithPath(path: PathString) = new IsNotLowerThenError(valueMax, value, isInclusive, path)

    def expected = valueMax

    override def parameters = Map("max" -> valueMax, "isInclusive" -> isInclusive)
  }

  sealed trait WrongSizeError extends DomainErrorWithExpectation

  class IsToSmallError(valueMin: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.tooSmallError", path) with WrongSizeError {

    def copyWithPath(path: PathString) = new IsToSmallError(valueMin, value, path)

    def expected = valueMin

    override def parameters = Map("expected" -> expected)
  }

  class IsToBigError(valueMax: Any, value: Any, path: PathString = Path./)
      extends AbstractDomainError(value, "error.dvalidation.tooBigError", path) with WrongSizeError {

    def copyWithPath(path: PathString) = new IsToBigError(valueMax, value, path)

    def expected = valueMax

    override def parameters = Map("expected" -> expected)
  }

  object CustomValidationError {
    def apply(value: Any, key: String, params: (String, Any)*) = new CustomValidationError(value, key, params.toMap)

    def withKey(e: DomainError, msgKey: String) = new CustomValidationError(e.value, msgKey, path = e.path, params = e.parameters)
  }

  class CustomValidationError(value: Any, key: String, params: Map[String, Any] = Map.empty, path: PathString = Path./) extends AbstractDomainError(value, key, path, params) {

    def copyWithPath(path: PathString) = new CustomValidationError(value, key, params, path)
  }

}
