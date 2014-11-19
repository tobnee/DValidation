package net.atinu.dvalidation.validator

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.{ IsToBigError, IsToSmallError, WrongSizeError }

import scala.annotation.implicitNotFound
import scala.collection.GenTraversableOnce
import scalaz.Foldable

trait SizedValidator extends ValidatorBase {

  /**
   * Check if the size of a value is in a specified range
   *
   * {{{
   *   hasSize(List(1, 2, 3), min = 3) === Success(List(1, 2, 3))
   * }}}
   *
   * @param value a value
   * @param min a minimal size (inclusive), default [[Int.MinValue]]
   * @param max a maximum size (inclusive), default [[Int.MaxValue]]
   * @param s a [[Sized]] view on the validation value
   * @return if size is outside of range then [[IsToBigError]] or [[IsToSmallError]]
   */
  def hasSize[T](value: T, min: Int = Int.MinValue, max: Int = Int.MaxValue)(implicit s: Sized[T], mapError: ErrorMap[WrongSizeError]): DValidation[T] = {
    if (max < min) throw new IllegalArgumentException(s"wrong validation definition min: $min >= max: $max")
    val size = s.size(value)
    if (size <= max) {
      if (size < min) failMapped(new IsToSmallError(min, size))
      else valid(value)
    } else failMapped(new IsToBigError(max, size))
  }

  /**
   * Check the length of a String
   * @see [[hasSize()]]
   */
  def hasLength(value: String, min: Int = Int.MinValue, max: Int = Int.MaxValue)(implicit mapError: ErrorMap[WrongSizeError]): DValidation[String] = {
    hasSize(value, min, max)(Sized.StringAsSized, mapError)
  }

  /**
   * View on types which have a size
   */
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
    import scala.language.higherKinds

    def sizeOf[T](s: T => Int): Sized[T] = new Sized[T] {
      def size(v: T) = s(v)
    }

    implicit def FoldableAsSized[A, T[A]](implicit f: Foldable[T]): Sized[T[A]] = new Sized[T[A]] {
      def size(v: T[A]): Int = f.count(v)
    }

    implicit object StringAsSized extends Sized[String] {
      def size(v: String): Int = v.length
    }
  }

}
