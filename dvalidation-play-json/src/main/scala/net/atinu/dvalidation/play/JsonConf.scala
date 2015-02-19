package net.atinu.dvalidation.play

import net.atinu.dvalidation.DomainError
import net.atinu.dvalidation.play.JsonConf.MappedValue.ValueMapper
import play.api.libs.json.Json.JsValueWrapper
import play.api.libs.json._

import scala.reflect.ClassTag

object JsonConf {

  private val EmptyObject = JsObject(Nil)

  type JsonPrinter = (DomainError => JsObject)

  protected trait PrinterFactory {
    def key: String
    def apply(v: JsValueWrapper) = Json.obj(key -> v)
  }

  trait NoPrinter extends JsonPrinter {
    def apply(error: DomainError) = EmptyObject
  }

  object PathPrinter extends PrinterFactory {
    val key = "path"
  }

  trait PathPrinter extends JsonPrinter

  object NoPathPrinter extends PathPrinter with NoPrinter

  /**
   * given a [[DomainError.path]] /a/b/c -> a string /a/b/c
   */
  object SlashSeparatedPath extends PathPrinter {
    import net.atinu.dvalidation.Path._
    def apply(error: DomainError) = PathPrinter(error.path.unwrap)
  }

  /**
   * given a [[DomainError.path]] /a/b/c -> an array  [a, b, c]
   */
  object PathAsArray extends PathPrinter {
    import net.atinu.dvalidation.Path._
    def apply(error: DomainError) =
      PathPrinter(error.path.segments.map(_.value))
  }

  object FieldPrinter extends PrinterFactory {
    val key = "field"
  }

  trait FieldPrinter extends JsonPrinter

  object NoFieldPrinter extends FieldPrinter with NoPrinter

  /**
   * given a [[DomainError.path]] /a/b/field -> a string field
   */
  object DefaultField extends FieldPrinter {
    import net.atinu.dvalidation.Path._
    def apply(error: DomainError) = {
      FieldPrinter(error.path.segments.last.value)
    }
  }

  object MsgKeyPrinter extends PrinterFactory {
    val key = "msgKey"
  }

  trait MsgKeyPrinter extends JsonPrinter

  object NoMsgKey extends MsgKeyPrinter with NoPrinter

  /**
   * given a [[DomainError.msgKey]] dvalidation.a.b -> a string dvalidation.a.b
   */
  object DefaultMsgKey extends MsgKeyPrinter {
    def apply(error: DomainError) =
      MsgKeyPrinter(error.msgKey)
  }

  object ValuePrinter extends PrinterFactory {
    val key = "value"
  }

  trait ValuePrinter extends JsonPrinter

  object NoValue extends ValuePrinter with NoPrinter

  /**
   * given a [[DomainError.value]] x -> a string x.toString
   */
  object ToStringValue extends ValuePrinter {
    def apply(error: DomainError): JsObject =
      ValuePrinter(error.value.toString)
  }

  def mappedValue(mapper: ValueMapper): MappedValue = {
    new MappedValue(mapper)
  }

  /**
   * map values of the given type to a JSON representation
   * @param w json translation rule
   * @tparam T given type
   */
  def mapValueToJson[T](implicit ct: ClassTag[T], w: Writes[T]): MappedValue = {
    mappedValue { case a: T => Json.toJson(a) }
  }

  object MappedValue {
    type ValueMapper = PartialFunction[Any, JsValue]
  }

  /**
   * Translate a [[DomainError.value]] to a Json Representation depending on the type or
   * value of the error value
   */
  class MappedValue(private val mapper: ValueMapper) extends ValuePrinter {

    /**
     * build a Json representation for a [[DomainError.value]]
     * @throws MatchError given a non defined mapping
     */
    def apply(error: DomainError): JsObject = {
      ValuePrinter(mapper.apply(error.value))
    }

    /**
     * compose with another [[MappedValue]]
     */
    def orElse(mappedValue: MappedValue) = {
      new MappedValue(mapper.orElse(mappedValue.mapper))
    }

    /**
     * Make the mapping complete by translating values which are not mapped to a String
     * @see [[ToStringValue.apply()]]
     */
    def withToStringDefault = {
      val pf: ValueMapper = { case e => JsString(e.toString) }
      new MappedValue(mapper.orElse(pf))
    }
  }

  object ArgsPrinter extends PrinterFactory {
    val key = "args"
  }

  trait ArgsPrinter extends JsonPrinter

  object NoArgs extends ArgsPrinter with NoPrinter

  /**
   * given a [[DomainError.args]] "a", "b", "c" -> an array ["a", "b", "c"]
   */
  object ArgsAsArray extends ArgsPrinter {
    def apply(error: DomainError) =
      if (error.args.isEmpty) EmptyObject
      else ArgsPrinter(error.args)
  }

  object MsgPrinter extends PrinterFactory {
    val key = "msg"
  }

  trait MsgPrinter extends JsonPrinter

  object NoMsgPrinter extends MsgPrinter with NoPrinter

  class TranslationMessagePrinter(lang: String, toMsg: (DomainError, String) => String) extends MsgPrinter {
    def apply(error: DomainError) = MsgPrinter(toMsg(error, lang))
  }

}
