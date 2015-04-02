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
  }

  def validateInScope[S](value: Any, scope: S)(sv: ScopedValidations[S]*)(implicit sd: Scope[S]): DValidation[_] = {
    var builder = List.newBuilder[DValidation[_]]
    for (s <- sv) { builder ++= s.validationsFor(scope) }
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

  trait PathScope {
    implicit def pathScope = new Scope[PathString] {
      import Path._

      def matches(parentScope: PathString, subScope: PathString): Boolean = {
        val ep = parentScope.elements
        val ec = subScope.elements
        ep.size <= ec.size && ec.take(ep.size) == ep
      }
    }
  }

  object PathScope extends PathScope

}
