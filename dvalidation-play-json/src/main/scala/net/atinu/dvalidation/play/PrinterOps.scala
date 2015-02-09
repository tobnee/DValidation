package net.atinu.dvalidation.play

import net.atinu.dvalidation.DomainError
import play.api.libs.json.{JsObject, JsString, Json}

object PrinterOps {

  private val EmptyObject = JsObject(Nil)

  type JsonPrinter = (DomainError => JsObject)

  trait NoPrinter extends PathPrinter {
    def apply(error: DomainError) = EmptyObject
  }

  trait PathPrinter extends JsonPrinter

  object NoPathPrinter extends PathPrinter with NoPrinter

  object SlashSeparated extends PathPrinter {
    import net.atinu.dvalidation.Path._
    def apply(error: DomainError) =
      Json.obj("path" -> error.path.path.unwrap)
  }

  trait FieldPrinter extends JsonPrinter

  object NoFieldPrinter extends FieldPrinter with NoPrinter

  object DefaultField extends FieldPrinter {
    import net.atinu.dvalidation.Path._
    def apply(error: DomainError) =
      Json.obj("field" -> error.path.segments.last.value)
  }

  trait MsgKeyPrinter extends JsonPrinter

  object NoMsgKey extends MsgKeyPrinter with NoPrinter

  object DefaultMsgKey extends MsgKeyPrinter {
    def apply(error: DomainError) =
      Json.obj("msgKey" -> error.msgKey)
  }

  trait ValuePrinter extends JsonPrinter

  object NoValue extends ValuePrinter with NoPrinter

  object ToStringValue extends ValuePrinter {
    def apply(error: DomainError): JsObject =
      Json.obj("value" -> JsString(error.value.toString))
  }

  trait ArgsPrinter extends JsonPrinter

  object NoArgs extends ArgsPrinter with NoPrinter

  object ArgsAsArray extends ArgsPrinter {
    def apply(error: DomainError) =
      Json.obj("args" -> error.args)
  }

  trait MsgPrinter extends JsonPrinter

  object NoMsgPrinter extends MsgPrinter with NoPrinter

  class TranslationMessageKey(lang: String, toMsg: (DomainError, String) => String) extends MsgKeyPrinter {
    def apply(error: DomainError) = Json.obj("msg" -> toMsg(error, lang))
  }

}
