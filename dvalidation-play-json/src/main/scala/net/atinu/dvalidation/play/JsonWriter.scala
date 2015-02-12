package net.atinu.dvalidation.play

import net.atinu.dvalidation.{ DomainError, DomainErrors }
import net.atinu.dvalidation.play.JsonWriterOps._

import play.api.libs.json._

object JsonWriter {

  def render(errors: DomainErrors): JsObject = {
    new JsonWriter().render(errors)
  }

  def renderSingle(errors: DomainError): JsObject = {
    new JsonWriter().renderSingle(errors)
  }
}

class JsonWriter(
    path: PathPrinter = JsonWriterOps.SlashSeparated,
    field: FieldPrinter = JsonWriterOps.NoFieldPrinter,
    msgKey: MsgKeyPrinter = JsonWriterOps.DefaultMsgKey,
    value: ValuePrinter = JsonWriterOps.ToStringValue,
    args: ArgsPrinter = JsonWriterOps.ArgsAsArray,
    msg: MsgPrinter = JsonWriterOps.NoMsgPrinter) {

  def render(errors: DomainErrors): JsObject = {
    val errorsJson = errors.asList.map { error => renderSingle(error) }
    Json.obj("errors" -> errorsJson)
  }

  def renderSingle(error: DomainError): JsObject = {
    path.apply(error) ++
      field.apply(error) ++
      msgKey.apply(error) ++
      value.apply(error) ++
      msg.apply(error) ++
      args.apply(error)
  }
}
