package net.atinu.dvalidation.scopes

import net.atinu.dvalidation._

case class ScopedValidations[T](scopes: List[T], scopeDef: Scope[T], validations: () => List[DValidation[_]]) {

  def matches(expectedScope: T): Boolean = scopes.exists(scope => scopeDef.matches(expectedScope, scope))

  def validationsFor(expectedScope: T): List[DValidation[_]] =
    if (matches(expectedScope)) validations.apply()
    else Nil

}
