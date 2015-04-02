package net.atinu.dvalidation

import net.atinu.dvalidation
import net.atinu.dvalidation.Path._

package object scopes {

  def allInScope[T](scope: T*)(v: => List[DValidation[_]])(implicit sd: Scope[T]) =
    ScopedValidations(scope.toList, sd, () => v)

  def inScope[T](scope: T*)(v: => DValidation[_])(implicit sd: Scope[T]) =
    ScopedValidations(scope.toList, sd, () => List(v))

  implicit class tToScopeValidation(val value: Any) extends AnyVal {

    def validateScoped[S](scope: S)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
      validateInScope(value, scope)(sv: _*)
    }

    def validateAnyScope[S](scopes: S*)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
      validateForAnyScope(value, scopes: _*)(sv: _*)
    }

    def validateAllScopes[S](scopes: S*)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
      validateForAllScopes(value, scopes: _*)(sv: _*)
    }
  }

  def validateInScope[S](value: Any, scope: S)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
    validateScopeWithOptions(value, sv)(_.validationsForScope(scope))
  }

  def validateForAnyScope[S](value: Any, scopes: S*)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
    validateScopeWithOptions(value, sv)(_.validationsForOneOf(scopes))
  }

  def validateForAllScopes[S](value: Any, scopes: S*)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
    validateScopeWithOptions(value, sv)(_.validationsForAllOf(scopes))
  }

  private def validateScopeWithOptions[S](value: Any, sv: Seq[ScopedValidations[S]])(f: ScopedValidations[S] => List[DValidation[_]]) = {
    var builder = List.newBuilder[DValidation[_]]
    for (s <- sv) { builder ++= f(s) }
    dvalidation.applyValidations(builder.result(), value)
  }

  trait Scope[T] {
    def matches(x: T, y: T): Boolean
  }

  trait SymbolScope {
    implicit def symbolScope = new Scope[Symbol] {
      def matches(v1: Symbol, v2: Symbol) = v1 == v2
    }
  }

  object SymbolScope extends SymbolScope

  trait EqualityScope {
    implicit def equalScope[T](implicit equ: scalaz.Equal[T]) = new Scope[T] {
      def matches(x: T, y: T): Boolean = equ.equal(x, y)
    }
  }

  object EqualityScope extends EqualityScope

  trait AnyEqualScope[T] extends Scope[T] {
    def matches(x: T, y: T): Boolean = x == y
  }

  object AnyEqualScope {
    def equalityScope[T] = new AnyEqualScope[T] {}
  }

  trait PathScope {
    implicit def pathScope = new Scope[PathString] {
      import net.atinu.dvalidation.Path._

      def matches(parentScope: PathString, subScope: PathString): Boolean = {
        subScope.unwrap startsWith parentScope.unwrap
      }
    }
  }

  object PathScope extends PathScope

}
