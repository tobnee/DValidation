package net.atinu.dvalidation.play

import net.atinu.dvalidation.{ Path, DValidation, DomainError, DomainErrors }
import net.atinu.dvalidation.play.JsonConf._

import play.api.libs.json._

import scalaz.{ Success, Failure, Validation }

object JsonWriter {

  def apply(
    path: PathPrinter = JsonConf.SlashSeparatedPath,
    field: FieldPrinter = JsonConf.NoFieldPrinter,
    msgKey: MsgKeyPrinter = JsonConf.DefaultMsgKey,
    value: ValuePrinter = JsonConf.ToStringValue,
    args: ArgsPrinter = JsonConf.NoArgs,
    msg: MsgPrinter = JsonConf.NoMsgPrinter,
    errorTransformer: ErrorTransformer = NoErrorTransformer) =
    new JsonWriter(path, field, msgKey, value, args, msg, errorTransformer)

  def applyH(
    field: FieldPrinter = JsonConf.NoFieldPrinter,
    msgKey: MsgKeyPrinter = JsonConf.DefaultMsgKey,
    value: ValuePrinter = JsonConf.ToStringValue,
    args: ArgsPrinter = JsonConf.NoArgs,
    msg: MsgPrinter = JsonConf.NoMsgPrinter,
    errorTransformer: ErrorTransformer = NoErrorTransformer) =
    new HJsonWriter(field, msgKey, value, args, msg, errorTransformer)

  val default: JsonWriter = apply()

  val hierarchical: HJsonWriter = applyH()
}

class HJsonWriter(field: FieldPrinter,
    msgKey: MsgKeyPrinter,
    value: ValuePrinter,
    args: ArgsPrinter,
    msg: MsgPrinter,
    errorTransformer: ErrorTransformer) {

  /**
   * All domain errors get mapped to a path hierarchy
   */
  def renderAll(errors: DomainErrors): JsObject = {
    import Path._
    errors.sorted.asList.map { err =>
      val pathArray = err.path.segments.map(_.value)
      pathArray.reverse match {
        case field +: tail =>
          val leafObject = Json.obj(field -> renderSingle(err))
          tail.foldLeft(leafObject)((sub, parentField) =>
            Json.obj(parentField -> sub.as[JsObject])
          )
        case _ => renderSingle(err)
      }
    }.reduce((a, b) => a.as[JsObject] deepMerge b.as[JsObject])
  }

  /**
   * Given the [[JsonConf]] class parameters represent a single [[DomainError]]
   */
  def renderSingle(error: DomainError): JsObject = {
    val res =
      field.apply(error) ++
        msgKey.apply(error) ++
        value.apply(error) ++
        msg.apply(error) ++
        args.apply(error)
    errorTransformer.apply(res, error)
  }

  /**
   * Given a [[scalaz.Failure]] return [[renderAll()]] within a JSON object or a JSON representation of the value object
   * @param errorRootName name of the root note containing the error values
   * @tparam T domain validation value type
   */
  def renderValidation[T: Writes](validation: DValidation[T], errorRootName: String = "errors"): JsValue = {
    validation match {
      case Failure(a) => Json.obj(errorRootName -> renderAll(a))
      case Success(b) => Json.toJson(b)
    }
  }

  /**
   * Transforms a [[DValidation]] into a validation with a JSON error and value type
   * @tparam T type of the [[DValidation]] value
   */
  def toJsonValidation[T: Writes](validation: DValidation[T]): Validation[JsObject, JsValue] = {
    validation.bimap(renderAll, value => Json.toJson(value))
  }

}

/**
 * A configurable and extendable JSON builder for [[DValidation]] related entities
 * @param path represent [[DomainError.path]]
 * @param field represent the last [[DomainError.path]] segment
 * @param msgKey represent [[DomainError.msgKey]]
 * @param value represent [[DomainError.value]]
 * @param args represent [[DomainError.args]]
 * @param msg represent a [[DomainError]] as string
 * @param errorTransformer replaces / adds / removes information based on their type or content
 */
class JsonWriter(
    path: PathPrinter,
    field: FieldPrinter,
    msgKey: MsgKeyPrinter,
    value: ValuePrinter,
    args: ArgsPrinter,
    msg: MsgPrinter,
    errorTransformer: ErrorTransformer) {

  /**
   * For each [[DomainError]] a corresponding entry in the [[JsArray]]
   * @see [[renderSingle()]]
   */
  def renderAll(errors: DomainErrors): JsArray = {
    JsArray(errors.asList.map { error => renderSingle(error) })
  }

  /**
   * Given the [[JsonConf]] class parameters represent a single [[DomainError]]
   */
  def renderSingle(error: DomainError): JsObject = {
    val res = path.apply(error) ++
      field.apply(error) ++
      msgKey.apply(error) ++
      value.apply(error) ++
      msg.apply(error) ++
      args.apply(error)
    errorTransformer.apply(res, error)
  }

  /**
   * Given a [[scalaz.Failure]] return an empty [[JsArray]] otherwise the result of [[renderAll()]]
   */
  def renderFailure(validation: DValidation[_]): JsArray =
    validation.errorView.map(renderAll).getOrElse(JsArray(Nil))

  /**
   * Given a [[scalaz.Failure]] return [[renderAll()]] within a JSON object or a JSON representation of the value object
   * @param errorRootName name of the root note containing the error values
   * @tparam T domain validation value type
   */
  def renderValidation[T: Writes](validation: DValidation[T], errorRootName: String = "errors"): JsValue = {
    validation match {
      case Failure(a) => Json.obj(errorRootName -> renderAll(a))
      case Success(b) => Json.toJson(b)
    }
  }

  /**
   * Transforms a [[DValidation]] into a validation with a JSON error and value type
   * @tparam T type of the [[DValidation]] value
   */
  def toJsonValidation[T: Writes](validation: DValidation[T]): Validation[JsArray, JsValue] = {
    validation.bimap(renderAll, value => Json.toJson(value))
  }

  def asWrites: Writes[DomainError] = DomainErrorWrites.customDomainErrorWrites(this)
}
