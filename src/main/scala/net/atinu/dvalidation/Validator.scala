package net.atinu.dvalidation

import net.atinu.dvalidation.Path.PathString

import scala.reflect.ClassTag
import scala.util.Try
import scalaz._
import scalaz.NonEmptyList._
import scalaz.syntax.validation._

object Validator {
  import syntax.semigroup._

  def notEmpty(s: String): DValidation[String] =
    if (s.isEmpty) new IsEmptyStringError().invalid else s.valid

  def hasElements[T <: Traversable[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsEmptySeqError().invalid else s.valid

  def isSome[T <: Option[_]](s: T): DValidation[T] =
    if (s.isEmpty) new IsNoneError().invalid else s.valid

  def isTrySuccess[T <: Try[_]](s: T): DValidation[T] =
    s match {
      case value: scala.util.Success[_] => s.success
      case scala.util.Failure(e) => new IsTryFailureError(e).invalid
    }

  def ensure[T](s: T)(key: String, args: Any*)(v: T => Boolean): DValidation[T] =
    if (v(s)) s.success else new CustomValidationError(s, key, args.map(_.toString)).invalid

  def validSequence[T](seq: Traversable[T], validator: DValidator[T]): IndexedSeq[DValidation[T]] = {
    seq.toIndexedSeq.zipWithIndex.map {
      case (value, idx) =>
        nestPathOnError(validator(value), _.nestIndex(idx))
    }
  }

  def validate[T](value: T)(cond: T => Boolean)(error: => DomainError): DValidation[T] =
    if (cond(value)) value.valid else error.invalid

  def template[T](v: DValidator[T]): DValidator[T] = v

  def invalid[T](value: Any, key: String) = new CustomValidationError(value, key).invalid[T]

  def invalid[T](value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq).invalid[T]

  def valid[T](value: T): DValidation[T] = value.valid

  implicit class ErrorToErrors(val error: DomainError) extends AnyVal {
    def invalid[T] = new DomainErrors(NonEmptyList.apply(error)).fail[T]
  }

  implicit class tToSuccess[T](val value: T) extends AnyVal {
    def valid: DValidation[T] = value.success[DomainErrors]
  }

  implicit class tToValidation[T](val value: T) extends AnyVal {
    def validateWith(validations: DValidation[_]*): DValidation[T] = {
      applyValidations(validations, value)
    }
  }

  implicit class tryToValidation[T](val value: Try[T]) extends AnyVal {
    def asValidation: DValidation[T] = isTrySuccess(value).map(_.get)
  }

  implicit class optToValidation[T](val value: Option[T]) extends AnyVal {
    def asValidation: DValidation[T] = isSome(value).map(_.get)
  }

  implicit class dSeqValidation[T](val value: IndexedSeq[DValidation[T]]) extends AnyVal {
    def forAttribute(attr: Symbol): IndexedSeq[DValidation[T]] = {
      value.map(validation => nestPathOnError(validation, _.nestAttribute(attr)))
    }
  }

  implicit class dValFirstSuccess[T](val value: DValidation[T]) extends AnyVal {

    def isValidOr[R <: T](next: => DValidation[R]) = value.findSuccess(next)

    def forAttribute(attr: Symbol): DValidation[T] = {
      nestPathOnError(value, _.nestAttribute(attr))
    }

    def errorView = value.fold(Option.apply, _ => None)

    def withValidations(validations: Seq[DValidation[_]]) =
      validateAll(validations, value)
  }

  private[dvalidation] def nestPathOnError[T](value: DValidation[T], nestPathTransform: DomainError => DomainError) = {
    value.leftMap(domainErrors => domainErrors.copy(errors =
      domainErrors.errors.map(error => nestPathTransform(error))))
  }

  private[dvalidation] def applyValidations[T](validations: Seq[DValidation[_]], value: T): DValidation[T] = {
    val validValue = valid(value)
    validateAll(validations, validValue)
  }

  private[dvalidation] def validateAll[T](validations: Seq[DValidation[_]], validValue: DValidation[T]): DValidation[T] = {

    def failed(e: Failure[DomainErrors, _]): DValidation[T] =
      e.asInstanceOf[DValidation[T]]

    validations.foldLeft(validValue) {
      case (Success(_), Success(_)) => validValue
      case (Success(_), e @ Failure(_)) => failed(e)
      case (Failure(e1), Failure(e2)) => (e1 |+| e2).fail
      case (e @ Failure(_), Success(_)) => failed(e)
    }
  }

  implicit def errorsSemiGroup: Semigroup[DomainErrors] =
    new Semigroup[DomainErrors] {
      def append(f1: DomainErrors, f2: => DomainErrors): DomainErrors = {
        val errors = Semigroup[NonEmptyList[DomainError]].append(f1.errors, f2.errors)
        new DomainErrors(errors)
      }
    }
}

object DomainErrors {
  def withSingleError(error: DomainError) = DomainErrors(NonEmptyList.apply(error))

  def withErrors(errors: DomainError*) =
    if (errors.isEmpty) throw new IllegalArgumentException("DomainErrors depend on at least one DomainError")
    else DomainErrors(NonEmptyList(errors.head, errors.tail: _*))
}

case class DomainErrors(errors: NonEmptyList[DomainError]) {

  def asList: List[DomainError] = errors.list

  def firstError: DomainError = errors.head

  def errorsOfType[T <: DomainError](implicit ct: ClassTag[T]): List[T] = {
    val runtimeClass = ct.runtimeClass
    errors.list.filter(error => runtimeClass.isInstance(error)).asInstanceOf[List[T]]
  }

  override def toString = errors.list.mkString(",")
  def prettyPrint = errors.list.mkString("-->\n", "\n", "\n<--")
}

sealed trait Path

object Path {

  import scalaz.Id._

  type PathString = String @@ Path

  val SingleSlash = wrap("/")

  implicit class StringToPath(val v: String) extends AnyVal {
    def asPath: PathString = Path.wrap(v)
  }

  private lazy val r = """(/{1}+)|((/{1}+)([^/]+/{1}+|[^/]+)*?[^/]+)""".r.pattern

  def isValidPath(a: String): Boolean = r.matcher(a).matches()

  def wrap(a: String): PathString =
    if (isValidPath(a)) wrapInternal(a)
    else throw new IllegalArgumentException(s"$a is not a valid path")

  private[dvalidation] def wrapInternal(a: String): @@[String, Path] = Tag[String, Path](a)

  def unwrap(a: PathString): String = Tag.unsubst[String, Id, Path](a)
}

trait DomainError {
  def value: Any
  def msgKey: String
  def path: PathString
  def nest(path: PathString): DomainError
  def nestAttribute(segment: Symbol): DomainError
  def nestIndex(index: Int): DomainError
}

abstract class AbstractDomainError(valueP: Any, msgKeyP: String, pathP: PathString = Path.SingleSlash, argsP: Seq[String] = Nil) extends DomainError {
  def value = valueP
  def msgKey = msgKeyP
  def path = pathP
  def args = argsP

  def copyWithPath(path: PathString): DomainError

  def nest(path: PathString): DomainError = {
    nestIntern(path.tail)
  }

  def nestIndex(index: Int): DomainError = {
    nestIntern(s"[$index]")
  }

  def nestAttribute(segment: Symbol): DomainError = {
    nestIntern(segment.name)
  }

  private def nestIntern(seg: String): DomainError = {
    val newPath = Path.unwrap(path) match {
      case "/" => s"/$seg"
      case _ => s"/$seg$path"
    }
    copyWithPath(Path.wrapInternal(newPath))
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
    java.util.Arrays.asList(value, msgKey, path, args).hashCode()
}

class IsEmptyStringError(path: PathString = Path.SingleSlash) extends AbstractDomainError("", "error.dvalidation.emptyString", path) {
  def copyWithPath(path: PathString) = new IsEmptyStringError(path)
}

class IsEmptySeqError(path: PathString = Path.SingleSlash) extends AbstractDomainError(Nil, "error.dvalidation.emptySeq", path) {

  def copyWithPath(path: PathString) = new IsEmptySeqError(path)
}

class IsNoneError(path: PathString = Path.SingleSlash) extends AbstractDomainError(None, "error.dvalidation.isNone", path) {

  def copyWithPath(path: PathString) = new IsNoneError(path)
}

class IsTryFailureError(value: Throwable, path: PathString = Path.SingleSlash) extends AbstractDomainError(value, "error.dvalidation.isTryFailue", path) {

  def copyWithPath(path: PathString) = new IsTryFailureError(value, path)
}

object CustomValidationError {
  def apply(value: Any, key: String, args: String*) = new CustomValidationError(value, key, args.toSeq)
}

class CustomValidationError(value: Any, key: String, args: Seq[String] = Nil, path: PathString = Path.SingleSlash) extends AbstractDomainError(value, key, path, args) {

  def copyWithPath(path: PathString) = new CustomValidationError(value, key, args, path)
}

