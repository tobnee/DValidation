package net.atinu.dvalidation

import scala.reflect.ClassTag
import scalaz._

object DomainErrors {
  import scalaz.std.string._
  import Path._

  def apply[A <: DomainError](h: A, t: A*) = new DomainErrors(NonEmptyList(h, t: _*))

  def withSingleError(error: DomainError) = new DomainErrors(NonEmptyList.apply(error))

  def withErrors(errors: DomainError*) =
    if (errors.isEmpty) throw new IllegalArgumentException("DomainErrors depends on at least one DomainError")
    else DomainErrors(errors.head, errors.tail: _*)

  def fromNel[T <: DomainError](nel: NonEmptyList[T]): DomainErrors = new DomainErrors(nel)

  def unapplySeq[A](e: DomainErrors): Option[(DomainError, List[DomainError])] =
    NonEmptyList.unapplySeq(e.errors)

  implicit def domainErrorsInstances =
    new Semigroup[DomainErrors] with Equal[DomainErrors] with Show[DomainErrors] {
      override def shows(f: DomainErrors) = f.toString

      def append(f1: DomainErrors, f2: => DomainErrors): DomainErrors = {
        f1 append f2
      }

      def equal(a1: DomainErrors, a2: DomainErrors): Boolean = a1 == a2
    }

  val MsgKeyOrder: Order[DomainError] = scalaz.Order.orderBy[DomainError, String](_.msgKey)

  val PathOrdering: Order[DomainError] = scalaz.Order.orderBy[DomainError, String](_.path.unwrap)

  val PathDepthOrdering: Order[DomainError] =
    scalaz.Order.order[DomainError] { (a, b) =>
      val dOrdering = comparePathDepth(a, b)
      if (dOrdering == Ordering.EQ) PathOrdering.apply(a, b)
      else dOrdering
    }

  private def comparePathDepth(a: DomainError, b: DomainError) = {
    val slash = '/'
    val sizeA = a.path.unwrap.count(_ == slash)
    val sizeB = b.path.unwrap.count(_ == slash)
    if (sizeA == sizeB) Ordering.EQ
    else if (sizeA < sizeB) Ordering.LT
    else Ordering.GT
  }
}

/**
 * Container for [[DomainError]]. Every instance of this class has to reference at least one
 * [[DomainError]].
 */
final class DomainErrors private (e: NonEmptyList[DomainError]) {
  import net.atinu.dvalidation.Path._
  import scalaz.syntax.equal._
  import scalaz.std.string._

  def errors: NonEmptyList[DomainError] = e

  def asList: List[DomainError] = errors.list

  def firstError: DomainError = errors.head

  def errorsOfType[T <: DomainError](implicit ct: ClassTag[T]): List[T] = {
    val runtimeClass = ct.runtimeClass
    errors.list.filter(error => runtimeClass.isInstance(error)).asInstanceOf[List[T]]
  }

  def selectType[T <: DomainError](implicit ct: ClassTag[T]): Option[DomainErrors] = {
    val runtimeClass = ct.runtimeClass
    select(error => runtimeClass.isInstance(error))
  }

  def selectValue(v: Any): Option[DomainErrors] =
    select(_.value == v)

  def selectMsgKey(msgKey: String): Option[DomainErrors] =
    select(_.msgKey === msgKey)

  def selectPath(path: PathString): Option[DomainErrors] =
    select(_.path === path)

  def selectPathPrefix(path: PathString): Option[DomainErrors] =
    select(error => error.path.unwrap.startsWith(path.unwrap))

  def select(pred: DomainError => Boolean): Option[DomainErrors] = {
    errors.list.filter(pred) match {
      case x :: xs => Some(DomainErrors.apply(x, xs: _*))
      case Nil => None
    }
  }

  def map(t: DomainError => DomainError): DomainErrors =
    new DomainErrors(errors.map(t))

  def flatMap(t: DomainError => DomainErrors): DomainErrors =
    new DomainErrors(errors.flatMap(t.andThen(_.errors)))

  def append(e: DomainErrors) =
    new DomainErrors(this.errors.append(e.errors))

  def sorted(implicit o: Order[DomainError] = DomainErrors.PathDepthOrdering) =
    new DomainErrors(errors.sorted)

  def sortBy[B](f: DomainError => B)(implicit o: Order[B]) =
    new DomainErrors(errors.sortBy(f))

  def sortWith(lt: (DomainError, DomainError) => Boolean) =
    new DomainErrors(errors.sortWith(lt))

  override def toString = errors.list.mkString(",")

  override def equals(value: Any) = value match {
    case v: DomainErrors =>
      this.errors == v.errors
    case _ => false
  }

  override def hashCode(): Int = errors.hashCode()

  def prettyPrint = errors.list.mkString("-->\n", "\n", "\n<--")
}

