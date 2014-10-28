package net.atinu.dvalidation

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.util.ValidationSuite

class ErrorMapSpec extends ValidationSuite {

  test("Error key can be mapped") {
    implicit val errorMap = new ErrorMap[IsEmptyStringError] {
      def apply(v1: IsEmptyStringError): DomainError = DomainError.wrapWithKey(v1, "a.b.c")
    }
    notBlank("") should beInvalidWithErrorProps("", "a.b.c", "/")
  }

  test("Error keys can be mapped with a factory") {
    implicit val mapEmptyString = ErrorMap.mapKey[IsEmptyStringError](key = "a.b.c")
    notBlank("") should beInvalidWithErrorProps("", "a.b.c", "/")
  }
}
