package net.atinu.dvalidation.play

import net.atinu.dvalidation.{ DValidation, DomainError, DomainErrors }
import net.atinu.dvalidation.play.JsonConf._

import play.api.libs.json._

import scalaz.{ Success, Failure, Validation }

object JsonWriter {

  val default: JsonWriter = new JsonWriter()
}

/**
 * A configurable and extendable JSON builder for [[DValidation]] related entities
 * @param path represent [[DomainError.path]]
 * @param field represent the last [[DomainError.path]] segment
 * @param msgKey represent [[DomainError.msgKey]]
 * @param value represent [[DomainError.value]]
 * @param args represent [[DomainError.args]]
 * @param msg represent a [[DomainError]] as string
 */
class JsonWriter(
    path: PathPrinter = JsonConf.SlashSeparatedPath,
    field: FieldPrinter = JsonConf.NoFieldPrinter,
    msgKey: MsgKeyPrinter = JsonConf.DefaultMsgKey,
    value: ValuePrinter = JsonConf.ToStringValue,
    args: ArgsPrinter = JsonConf.ArgsAsArray,
    msg: MsgPrinter = JsonConf.NoMsgPrinter) {

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
    path.apply(error) ++
      field.apply(error) ++
      msgKey.apply(error) ++
      value.apply(error) ++
      msg.apply(error) ++
      args.apply(error)
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
