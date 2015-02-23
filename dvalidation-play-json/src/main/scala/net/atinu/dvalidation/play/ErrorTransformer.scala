package net.atinu.dvalidation.play

import net.atinu.dvalidation.DomainError
import play.api.libs.json.JsObject

import scala.reflect.ClassTag

object ErrorTransformer {

  /**
   * A transformation for a given type T. 
   * @param transFunc given the current state and the domain error yield a domain error
   */
  def transformFor[T <: DomainError](transFunc: (JsObject, T) => JsObject)(implicit ct: ClassTag[T]): ErrorTransformer =
    new ByTypeErrorTransformer[T] {
      def transform(currentState: JsObject, error: T) = transFunc(currentState, error)
    }

  /**
   * A dynamic transformation can be described by a partial function and should be used where [[transformFor()]] is
   * too static
   */
  def transformDynamic(pf: PartialFunction[(JsObject, DomainError), JsObject]): ErrorTransformer = {
    new DynamicErrorTransformer(pf)
  }

  /**
   * Aggregate 1 or more [[ErrorTransformer]].
   * @return A [[ErrorTransformer]] where only the first matching ([[ErrorTransformer.canTransform()]]) transformation
   *         is applied
   */
  def transformAllWith(transformers: Seq[ErrorTransformer]): ErrorTransformer =
    new AggregateAllErrorTransformer(transformers)

  /**
   * Aggregate 1 or more [[ErrorTransformer]].
   * @return A [[ErrorTransformer]] where all matching transformers are applied. This can lead to multible
   *         transformations for one error
   */
  def transformFirstWith(transformers: Seq[ErrorTransformer]): ErrorTransformer =
    new AggregateFirstErrorTransformer(transformers)
}

/**
 * Transform a JSON representation of a [[DomainError]]
 */
trait ErrorTransformer {

  /**
   * @param currentState the current JSON serialisation of the [[DomainError]]
   * @param error the domain error which led to the JSON
   * @return a new JSON representing the domain error
   */
  def apply(currentState: JsObject, error: DomainError): JsObject

  /**
   * Does the transformation yield a new result 
   */
  def canTransform(currentState: JsObject, error: DomainError): Boolean
}

object NoErrorTransformer extends ErrorTransformer {
  def apply(currentState: JsObject, error: DomainError): JsObject =
    currentState

  def canTransform(currentState: JsObject, error: DomainError) = false
}

trait AggregateTransformer extends ErrorTransformer {
  def transformers: Seq[ErrorTransformer]

  def canTransform(currentState: JsObject, error: DomainError) =
    transformers.exists(_.canTransform(currentState, error))
}

class AggregateAllErrorTransformer(val transformers: Seq[ErrorTransformer]) extends AggregateTransformer {

  def apply(currentState: JsObject, error: DomainError): JsObject =
    transformers.foldLeft(currentState) { (state, transformer) =>
      transformer(state, error)
    }

}

class AggregateFirstErrorTransformer(val transformers: Seq[ErrorTransformer]) extends AggregateTransformer {

  def apply(currentState: JsObject, error: DomainError): JsObject =
    transformers
      .find(_.canTransform(currentState, error))
      .map(_.apply(currentState, error))
      .getOrElse(currentState)

}

abstract class ByTypeErrorTransformer[T <: DomainError](implicit ct: ClassTag[T]) extends ErrorTransformer {

  def apply(currentState: JsObject, error: DomainError) = error match {
    case e: T => transform(currentState, e)
    case e => currentState
  }

  def canTransform(currentState: JsObject, error: DomainError) =
    ct.runtimeClass.isInstance(error)

  def transform(currentState: JsObject, error: T): JsObject

}

class DynamicErrorTransformer(pf: PartialFunction[(JsObject, DomainError), JsObject])
    extends ErrorTransformer {

  def apply(currentState: JsObject, error: DomainError) =
    if (canTransform(currentState, error)) pf(currentState, error)
    else currentState

  def canTransform(currentState: JsObject, error: DomainError) =
    pf.isDefinedAt(currentState, error)

}