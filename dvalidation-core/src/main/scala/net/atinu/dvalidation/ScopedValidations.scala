package net.atinu.dvalidation

import scala.reflect.ClassTag

case class ScopedValidations[T](sx: List[T], s: Scope[T], validations: () => List[DValidation[_]]) {
  def matchesStrict(x: T): Boolean = sx.exists(v => s.matchesStrict(v, x))

  def matches(x: Any): Boolean = sx.exists(v => s.matches(v, x))
}

trait Scope[T] {

  def matchesStrict(x: T, y: T): Boolean

  def matches(x: Any, y: Any): Boolean
}

object ScopedValidations {

  implicit def symbolScope = new Scope[Symbol] {
    def matchesStrict(v1: Symbol, v2: Symbol) = v1 == v2

    def matches(x: Any, y: Any): Boolean = (x, y) match {
      case (a: Symbol, b: Symbol) => matchesStrict(a, b)
      case _ => false
    }
  }

  implicit def equalScope[T](implicit equ: scalaz.Equal[T], manifest: ClassTag[T]) = new Scope[T] {
    val clazz = manifest.runtimeClass

    override def matchesStrict(x: T, y: T): Boolean = equ.equal(x, y)

    override def matches(x: Any, y: Any): Boolean = {
      if (clazz.isInstance(x) && clazz.isInstance(y))
        matchesStrict(x.asInstanceOf[T], y.asInstanceOf[T])
      else false
    }
  }

  def inScope(scope: Symbol*)(v: => DValidation[_]) =
    ScopedValidations(scope.toList, symbolScope, () => List(v))

  def allInScope(scope: Symbol*)(v: => List[DValidation[_]]) =
    ScopedValidations(scope.toList, symbolScope, () => v)

  def inCustomScope[T](scope: T*)(v: => DValidation[_])(implicit sd: Scope[T]) =
    ScopedValidations(scope.toList, sd, () => List(v))

}
