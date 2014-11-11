package net.atinu.dvalidation.validator

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.{ IsToBigError, IsToSmallError, WrongSizeError }

import scala.annotation.implicitNotFound
import scala.collection.GenTraversableOnce
import scalaz.Foldable

trait SizedValidator extends ValidatorBase {

  def hasSize[T](value: T, min: Int = Int.MinValue, max: Int = Int.MaxValue)(implicit f: Sized[T], mapError: ErrorMap[WrongSizeError]): DValidation[T] = {
    if (max < min) throw new IllegalArgumentException(s"wrong validation definition min: $min >= max: $max")
    val size = f.size(value)
    if (size <= max) {
      if (size < min) failMapped(new IsToSmallError(min, size))
      else valid(value)
    } else failMapped(new IsToBigError(max, size))
  }

  def hasLength(value: String, min: Int = Int.MinValue, max: Int = Int.MaxValue)(mapError: ErrorMap[WrongSizeError]): DValidation[String] = {
    hasSize(value, min, max)(Sized.StringAsSized, mapError)
  }

  @implicitNotFound(msg = "Cannot find implicit Validator.Sized type class for ${T}. " +
    "You can provide your own Instance or a scalaz.Foldable")
  trait Sized[T] {
    def size(v: T): Int
  }

  trait SizedLowPrioImplicits {
    implicit def CollectionsAsSized[T <: GenTraversableOnce[_]] = new Sized[T] {
      def size(v: T): Int = v.size
    }
  }

  object Sized extends SizedLowPrioImplicits {

    def sizeOf[T](s: T => Int): Sized[T] = new Sized[T] {
      def size(v: T) = s(v)
    }

    implicit def FoldableAsSized[A, T[A]](implicit f: Foldable[T]): Sized[T[A]] = new Sized[T[A]] {
      def size(v: T[A]): Int = f.length(v)
    }

    implicit object StringAsSized extends Sized[String] {
      def size(v: String): Int = v.length
    }
  }

}
