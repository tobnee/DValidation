package net.atinu.dvalidation.play

import net.atinu.dvalidation.errors.{IsEmptyStringError, IsNoneError}
import net.atinu.dvalidation.play.util.JsonMatcher
import org.scalatest.{FunSuite, Matchers}
import play.api.libs.json.JsString

class ErrorTransformerTest extends FunSuite with Matchers with JsonMatcher {

  test("A ErrorTransformer can change the JSON respresentation") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et = ErrorTransformer.transformFor[IsEmptyStringError]((res, error) => res + ("foo", JsString("bar")))
    val single = JsonWriter(errorTransformer = et).renderSingle(e1)
    single should containKeyValue("foo" -> "bar")
  }

  test("A ErrorTransformer does only work for its assinged type") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et = ErrorTransformer.transformFor[IsNoneError]((res, error) => res + ("foo", JsString("bar")))
    val single = JsonWriter(errorTransformer = et).renderSingle(e1)
    single should haveNoKey("foo")
  }

  test("A dynamic ErrorTransformer can change the JSON respresentation") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et = ErrorTransformer.transformDynamic { case (res, error: IsEmptyStringError) => res + ("foo", JsString("bar")) }
    val single = JsonWriter(errorTransformer = et).renderSingle(e1)
    single should containKeyValue("foo" -> "bar")
  }

  test("A dynamic ErrorTransformer does only work for its range") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et = ErrorTransformer.transformDynamic { case (res, error: IsNoneError) => res + ("foo", JsString("bar")) }
    val single = JsonWriter(errorTransformer = et).renderSingle(e1)
    single should haveNoKey("foo")
  }

  test("An aggregate first ErrorTransformer applies the first transformation") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et1 = ErrorTransformer.transformFor[IsEmptyStringError]((res, error) => res + ("foo1", JsString("bar")))
    val et2 = ErrorTransformer.transformFor[IsEmptyStringError]((res, error) => res + ("foo2", JsString("bar")))
    val single = JsonWriter(errorTransformer = ErrorTransformer.transformFirstWith(Seq(et1, et2))).renderSingle(e1)
    single should containKeyValue("foo1" -> "bar")
    single should haveNoKey("foo2")
  }

  test("An aggregate all ErrorTransformer applies all transformations") {
    import net.atinu.dvalidation.Path._
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val et1 = ErrorTransformer.transformFor[IsEmptyStringError]((res, error) => res + ("foo1", JsString("bar")))
    val et2 = ErrorTransformer.transformFor[IsEmptyStringError]((res, error) => res + ("foo2", JsString("bar")))
    val single = JsonWriter(errorTransformer = ErrorTransformer.transformAllWith(Seq(et1, et2))).renderSingle(e1)
    single should containKeyValue("foo1" -> "bar")
    single should containKeyValue("foo2" -> "bar")
  }

}
