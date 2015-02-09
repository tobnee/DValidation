package net.atinu.dvalidation.play

import net.atinu.dvalidation.DomainErrors
import net.atinu.dvalidation.play.PrinterOps._

import play.api.libs.json._

object Printer {

  def print(errors: DomainErrors): JsObject = {
    new Printer().print(errors)
  }

}

class Printer(
    path: PathPrinter = PrinterOps.SlashSeparated,
    field: FieldPrinter = PrinterOps.NoFieldPrinter,
    msgKey: MsgKeyPrinter = PrinterOps.DefaultMsgKey,
    value: ValuePrinter = PrinterOps.ToStringValue,
    args: ArgsPrinter = PrinterOps.ArgsAsArray,
    msg: MsgPrinter = PrinterOps.NoMsgPrinter) {

  def print(errors: DomainErrors): JsObject = {
    val errorsJson = errors.asList.map { error =>
      path.apply(error) ++
        field.apply(error) ++
        msgKey.apply(error) ++
        value.apply(error) ++
        msg.apply(error) ++
        args.apply(error)
    }
    Json.obj("errors" -> errorsJson)
  }
}
