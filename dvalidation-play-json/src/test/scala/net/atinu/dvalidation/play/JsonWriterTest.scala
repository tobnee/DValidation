package net.atinu.dvalidation.play

import net.atinu.dvalidation._
import net.atinu.dvalidation.errors.IsEmptyStringError
import net.atinu.dvalidation.play.JsonWriterOps.TranslationMessagePrinter
import net.atinu.dvalidation.play.util.JsonMatcher
import org.scalatest.{ FunSuite, Matchers }

class JsonWriterTest extends FunSuite with Matchers with JsonMatcher {

  test("Should render a single error to json") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val single = JsonWriter.renderSingle(e1)
    single should containKeyValue("path" -> "/tests/[0]/b")
    single should containKeyValue("msgKey" -> "error.dvalidation.emptyString")
    single should containKeyValue("value" -> "")
    single should haveNoKey("args")
  }

  test("Should render a field of a single value") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val single = new JsonWriter(field = JsonWriterOps.DefaultField).renderSingle(e1)
    single should containKeyValue("field" -> "b")
  }

  test("Should render a translated value") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val translator = new TranslationMessagePrinter("de", (error, lang) => s"error message $lang ${error.msgKey}")
    val single = new JsonWriter(msg = translator).renderSingle(e1)
    single should containKeyValue("msg" -> "error message de error.dvalidation.emptyString")
  }

}
