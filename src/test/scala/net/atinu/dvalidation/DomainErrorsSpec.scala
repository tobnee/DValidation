package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation.Path._

class DomainErrorsSpec extends ValidationSuite {

  test("DomainErrors can be filtered by type") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).errorsOfType[IsNoneError] should contain(new IsNoneError("/tests/[0]/c".asPath))
  }

  test("can map over domain errors") {
    DomainErrors
      .withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath))
      .map(error => new IsEmptyStringError("/tests/[0]".asPath)) should equal(DomainErrors
        .withSingleError(new IsEmptyStringError("/tests/[0]".asPath)))
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
    val e = DomainErrors.withErrors(
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
}
