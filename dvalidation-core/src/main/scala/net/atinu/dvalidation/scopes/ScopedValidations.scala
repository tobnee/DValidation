package net.atinu.dvalidation.scopes

import net.atinu.dvalidation._

case class ScopedValidations[T](scopes: List[T], scopeDef: Scope[T], validations: () => List[DValidation[_]]) {

  def matches(expectedScope: T): Boolean = scopes.exists(scope => scopeDef.matches(expectedScope, scope))

  def validationsForOneOf(expectedScopes: Seq[T]): List[DValidation[_]] =
    applyCond(expectedScopes.exists(matches))

  def validationsForAllOf(expectedScopes: Seq[T]): List[DValidation[_]] =
    applyCond(expectedScopes.forall(matches))

  def validationsForScope(expectedScope: T): List[DValidation[_]] =
    applyCond(matches(expectedScope))

  private def applyCond(p: Boolean) = {
    if (p) validations.apply()
    else Nil
  }

}
