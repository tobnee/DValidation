package net.atinu.dvalidation.play

import net.atinu.dvalidation.{ DValidation, DomainError, DomainErrors }
import net.atinu.dvalidation.play.JsonConf._

import play.api.libs.json._

object JsonWriter {

  def renderValidation(validation: DValidation[_]): JsArray =
    new JsonWriter().renderValidation(validation)

  def renderSingle(errors: DomainError): JsObject = {
    new JsonWriter().renderSingle(errors)
  }

  def renderAll(errors: DomainErrors): JsArray = {
    new JsonWriter().renderAll(errors)
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
  def renderValidation(validation: DValidation[_]): JsArray =
    validation.errorView.map(renderAll).getOrElse(JsArray(Nil))
}
