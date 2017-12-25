package net.atinu.dvalidation

import scala.reflect.ClassTag
import scalaz.{ Equal, NonEmptyList, Semigroup, Show }

object DomainErrors {

  def apply[A <: DomainError](h: A, t: A*) = new DomainErrors(NonEmptyList(h, t: _*))

  def withSingleError(error: DomainError) = new DomainErrors(NonEmptyList.apply(error))

  def withErrors(errors: DomainError*) =
    if (errors.isEmpty) throw new IllegalArgumentException("DomainErrors depend on at least one DomainError")
    else DomainErrors(errors.head, errors.tail: _*)

  def fromNel(nel: NonEmptyList[DomainError]): DomainErrors = new DomainErrors(nel)

  def unapplySeq[A](e: DomainErrors): Option[(DomainError, List[DomainError])] =
    NonEmptyList.unapply(e.errors).map { case (e, errors) => (e, errors.toList) }

  implicit def domainErrorsInstances =
    new Semigroup[DomainErrors] with Equal[DomainErrors] with Show[DomainErrors] {
      override def shows(f: DomainErrors) = f.toString

      def append(f1: DomainErrors, f2: => DomainErrors): DomainErrors = {
        f1 append f2
      }

      def equal(a1: DomainErrors, a2: DomainErrors): Boolean = a1 == a2
    }
}

/**
 * Container for [[DomainError]]. Every instance of this class has to reference at least one
 * [[DomainError]].
 */
final class DomainErrors private (e: NonEmptyList[DomainError]) {

  def errors: NonEmptyList[DomainError] = e

  def asList: List[DomainError] = errors.list.toList

  def firstError: DomainError = errors.head

  def errorsOfType[T <: DomainError](implicit ct: ClassTag[T]): List[T] = {
    val runtimeClass = ct.runtimeClass
    errors.list.toList.filter(error => runtimeClass.isInstance(error)).asInstanceOf[List[T]]
  }

  def map(t: DomainError => DomainError): DomainErrors =
    new DomainErrors(errors.map(t))

  def flatMap(t: DomainError => DomainErrors): DomainErrors =
    new DomainErrors(errors.flatMap(t.andThen(_.errors)))

  def append(e: DomainErrors) =
    new DomainErrors(this.errors.append(e.errors))

  override def toString = errors.list.toList.mkString(",")

  override def equals(value: Any) = value match {
    case v: DomainErrors =>
      this.errors == v.errors
    case _ => false
  }

  override def hashCode(): Int = errors.hashCode()

  def prettyPrint = errors.list.toList.mkString("-->\n", "\n", "\n<--")
}

