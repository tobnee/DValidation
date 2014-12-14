package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation.errors._
import net.atinu.dvalidation.Path._

class DomainErrorsSpec extends ValidationSuite {

  test("DomainErrors can be filtered by type (list)") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).errorsOfType[IsNoneError] should contain(new IsNoneError("/tests/[0]/c".asPath))
  }

  test("DomainErrors can be filtered by type") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).selectType[IsNoneError] should equal(Some(DomainErrors.withSingleError(new IsNoneError("/tests/[0]/c".asPath))))
  }

  test("DomainErrors can be filtered by key") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).selectMsgKey("error.dvalidation.emptyString") should equal(Some(DomainErrors.withErrors(
        new IsEmptyStringError("/tests/[0]/b".asPath),
        new IsEmptyStringError("/tests/[1]/b".asPath)
      )))
  }

  test("DomainErrors can be filtered by value") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).selectValue(None) should equal(Some(DomainErrors.withErrors(
        new IsNoneError("/tests/[0]/c".asPath)
      )))
  }

  test("DomainErrors can be filtered by path") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).selectPath("/tests/[0]/c".asPath) should equal(Some(DomainErrors.withErrors(
        new IsNoneError("/tests/[0]/c".asPath)
      )))
  }

  test("DomainErrors can be filtered by path prefix") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).selectPathPrefix("/tests/[0]".asPath) should equal(Some(DomainErrors.withErrors(
        new IsEmptyStringError("/tests/[0]/b".asPath),
        new IsNoneError("/tests/[0]/c".asPath)
      )))
  }

  test("can map over domain errors") {
    val newError = CustomValidationError("", "dvalidation.error.emptyName").copyWithPath("/tests/[0]".asPath)
    DomainErrors
      .withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath))
      .map(error => newError) should equal(DomainErrors
        .withSingleError(newError))
  }

  test("can flatMap over domain errors") {
    DomainErrors
      .withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath))
      .flatMap(error => DomainErrors.withSingleError(new IsEmptyStringError("/tests/[0]".asPath))) should equal(DomainErrors
        .withSingleError(new IsEmptyStringError("/tests/[0]".asPath)))
  }

  test("can append two domain errors") {
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val es1 = DomainErrors
      .withSingleError(e1)
    val e2 = new IsEmptyStringError("/tests/[1]/b".asPath)
    val es2 = DomainErrors
      .withSingleError(e2)
    es1.append(es2) should equal(DomainErrors.withErrors(e1, e2))
  }

  test("has a sequence extractor API") {
    val e: DomainErrors = DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    )
    val a = e match {
      case DomainErrors(e1, as @ _*) => e1
    }
    a should equal(new IsEmptyStringError("/tests/[0]/b".asPath))
  }

  test("has an element extractor API") {
    val DomainError(value, key, _, _) = new CustomValidationError("value", "key")
    key should equal("key")
    value should equal("value")
  }

  test("Can sort without specifying order") {
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val e2 = new IsNoneError("/tests/[0]/c".asPath)
    val e3 = new IsEmptyStringError("/tests/[1]/b".asPath)
    val e = DomainErrors.withErrors(e2, e1, e3)
    e.sorted.asList should contain inOrder (e1, e2, e3)
  }

  test("Can sort by specifying order") {
    val e1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val e2 = new IsNoneError("/tests/[0]/c".asPath)
    val e3 = new IsNotEqualError(2, 3)
    val e = DomainErrors.withErrors(e2, e1, e3)
    e.sorted(DomainErrors.MsgKeyOrder).asList should contain inOrder (e1, e2, e3)
  }

  test("Depth of path is considered in order") {
    val e1 = new IsEmptyStringError("/uestsdfd".asPath)
    val e2 = new IsNoneError("/tests/[0]".asPath)
    val e3 = new IsNoneError("/tests/[0]/df".asPath)
    val e4 = new IsNoneError("/a/b/c/d".asPath)
    val e = DomainErrors.withErrors(e4, e2, e1, e3)
    e.sorted.asList should contain inOrder (e1, e2, e3, e4)
  }
}
