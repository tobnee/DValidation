package net.atinu.dvalidation.play

import _root_.play.api.libs.json._
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.{ IsToSmallError, IsTryFailureError, IsNoneError, IsEmptyStringError }
import net.atinu.dvalidation.play.JsonConf.{ MappedValue, TranslationMessagePrinter }
import net.atinu.dvalidation.play.util.JsonMatcher
import org.scalatest.{ FunSuite, Matchers }

import scalaz.{ Success, Validation }

class JsonWriterTest extends FunSuite with Matchers with JsonMatcher {

  test("Should render a single error to json") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val single = JsonWriter.default.renderSingle(e1)
    single should containKeyValue("path" -> "/tests/[0]/b")
    single should containKeyValue("msgKey" -> "error.dvalidation.emptyString")
    single should containKeyValue("value" -> "")
    single should haveNoKey("args")
  }

  test("Should render a field of a single value") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val single = JsonWriter(field = JsonConf.DefaultField).renderSingle(e1)
    single should containKeyValue("field" -> "b")
  }

  test("Should render a failed validation") {
    import net.atinu.dvalidation.Path._
    val e1: DValidation[String] =
      Validation.failure(DomainErrors.withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath)))
    val res = JsonWriter.default.renderValidation(e1)
    (res \ "errors").asOpt[JsArray] should not be 'empty
    (res \ "errors").as[JsArray].value.head.as[JsObject] should containKeyValue("value" -> "")
  }

  test("Should render a sucessful validation") {
    val e1: DValidation[String] = Success("abc")
    val res = JsonWriter.default.renderValidation(e1)
    res.as[String] should equal("abc")
  }

  test("Should render a translated value") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val translator = new TranslationMessagePrinter("de", (error, lang) => s"error message $lang ${error.msgKey}")
    val single = JsonWriter(msg = translator).renderSingle(e1)
    single should containKeyValue("msg" -> "error message de error.dvalidation.emptyString")
  }

  test("Should render path as array") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val single = JsonWriter(path = JsonConf.PathAsArray).renderSingle(e1)
    single should containKeyValue("path" -> Json.arr("tests", "0", "b"))
  }

  test("Should render expected values") {
    val e1 = new IsToSmallError(2, 1)
    val single = JsonWriter.default.renderSingle(e1)
    single should containKeyValue("expected" -> "2")
  }

  test("Default value renderer returns None as string") {
    val error = JsonWriter.default.renderSingle(new IsNoneError())
    error should containKeyValue("value" -> "None")
  }

  test("Mapped value renderer returns None as empty object") {
    val mapper: MappedValue = JsonConf.mappedValue {
      case None => JsObject(Nil)
    }
    val error = JsonWriter(value = mapper).renderSingle(new IsNoneError())
    error should containKeyValue("value" -> JsObject(Nil))
  }

  test("Mapped value renderer can have a toString backup") {
    val mapper = JsonConf.mappedValue { case e: String => JsObject(Nil) }.withToStringDefault
    val error = JsonWriter(value = mapper).renderSingle(new IsNoneError())
    error should containKeyValue("value" -> "None")
  }

  test("Mapped value renderer can map values to JSON given a Writes") {
    implicit val writes: Writes[IllegalArgumentException] = Writes.apply(e => JsString(e.getMessage))
    val mapper = JsonConf.mapValueToJson[IllegalArgumentException]
    val error = JsonWriter(value = mapper).renderSingle(new IsTryFailureError(new IllegalArgumentException("foo")))
    error should containKeyValue("value" -> "foo")
  }

  test("Mapped value renderer can be composed") {
    implicit val writes: Writes[IllegalArgumentException] = Writes.apply(e => JsString(e.getMessage))
    val mapper1 = JsonConf.mapValueToJson[IllegalArgumentException]
    val mapper2 = JsonConf.mappedValue { case None => JsObject(Nil) }
    val mapper = mapper1 orElse mapper2
    val error = JsonWriter(value = mapper).renderSingle(new IsTryFailureError(new IllegalArgumentException("foo")))
    val error2 = JsonWriter(value = mapper).renderSingle(new IsNoneError())
    error should containKeyValue("value" -> "foo")
    error2 should containKeyValue("value" -> JsObject(Nil))
  }

  test("Depth of path is considered in order") {
    import Path._
    val e1 = new IsEmptyStringError("/a".asPath)
    val e2 = new IsNoneError("/tests/[0]".asPath)
    val e3 = new IsNoneError("/tests/[0]/df".asPath)
    val e4 = new IsNoneError("/a/b/c/d".asPath)
    val e = DomainErrors.withErrors(e4, e2, e1, e3)
    println(Json.prettyPrint(JsonWriter.applyH().renderAll(e)))
  }

}
