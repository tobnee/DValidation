package net.atinu.dvalidation

import net.atinu.dvalidation.Path.PathString

import scala.reflect.ClassTag

case class ScopedValidations[T](scopes: List[T], scopeDef: Scope[T], validations: () => List[DValidation[_]]) {

  def matches(expected: T): Boolean = scopes.exists(scope => scopeDef.matches(expected, scope))

  def validationsFor(expected: T): List[DValidation[_]] =
    if (matches(expected)) validations.apply()
    else Nil

}

trait Scope[T] {

  def matches(x: T, y: T): Boolean

}

object ScopedValidations {

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
        if (ep.size <= ec.size) ec.take(ep.size) == ep
        else false
      }
    }
  }

  object PathScope extends PathScope

  def inScope(scope: Symbol*)(v: => DValidation[_]) =
    ScopedValidations(scope.toList, SymbolScope.symbolScope, () => List(v))

  def allInScope(scope: Symbol*)(v: => List[DValidation[_]]) =
    ScopedValidations(scope.toList, SymbolScope.symbolScope, () => v)

  def inCustomScope[T](scope: T*)(v: => DValidation[_])(implicit sd: Scope[T]) =
    ScopedValidations(scope.toList, sd, () => List(v))

}
