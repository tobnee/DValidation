package net.atinu.dvalidation.validator

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.{ IsEmptyError, IsZeroError }

import scalaz.{ IsEmpty, Unapply, Equal, Monoid }

object GenericValidator extends GenericValidator

trait GenericValidator extends ValidatorBase {

  /**
   * Check if a value is a [[Monoid.zero]]
   *
   *  {{{
   *     import scalaz.std.anyVal._
   * notZero(1) === scalaz.Success(1)
   *  }}}
   *
   * @see [[IsZeroError]]
   */
  def notZero[T](s: T)(implicit m: Monoid[T], e: Equal[T], mapError: ErrorMap[IsZeroError]): DValidation[T] =
    if (m.isMZero(s)) failMapped(new IsZeroError(s)) else s.valid

  /**
   * Check if a value is empty in a generic way
   *
   * @param ma a value to be tested for emptiness
   * @tparam MA a shape supported by [[scalaz.Unapply]]
   * @see [[IsEmptyError]]
   */
  def nonEmptyGeneric[MA](ma: MA)(implicit U: Unapply[IsEmpty, MA], mapError: ErrorMap[IsEmptyError]): DValidation[MA] = {
    if (U.TC.isEmpty(U(ma))) failMapped(new IsEmptyError(ma))
    else valid(ma)
  }

}
