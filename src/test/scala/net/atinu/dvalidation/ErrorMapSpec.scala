package net.atinu.dvalidation

import Path._
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.util.ValidationSuite

class ErrorMapSpec extends ValidationSuite {

  import scalaz.std.anyVal._

  test("Error key can be mapped") {
    implicit val errorMap = new ErrorMap[IsEmptyStringError] {
      def apply(v1: IsEmptyStringError): DomainError = CustomValidationError.withKey(v1, "a.b.c")
    }
    notBlank("") should beInvalidWithErrorProps("", "a.b.c", "/")
  }

  test("Error key can be mapped with a factory") {
    implicit val mapEmptyString = ErrorMap.mapKey[IsEmptyStringError](key = "a.b.c")
    notBlank("") should beInvalidWithErrorProps("", "a.b.c", "/")
  }

  test("Error can be mapped") {
    implicit val mapEmptyString = new ErrorMap[IsEmptyStringError] {
      def apply(v1: IsEmptyStringError) = new CustomValidationError(v1.value, "a.b.c", v1.args, v1.path)
    }

    notBlank("") should beInvalidWithError(new CustomValidationError("", "a.b.c"))
  }

  test("Error can be mapped with a factory") {
    implicit val mapEmptyString = ErrorMap.apply[IsEmptyStringError](e =>
      new CustomValidationError(e.value, "a.b.c", e.args, e.path))

    notBlank("") should beInvalidWithError(new CustomValidationError("", "a.b.c"))
  }

  test("Error can be mapped with a factory - 2") {
    notBlank("")(ErrorMap.apply(e =>
      new CustomValidationError(e.value, "a.b.c", e.args, e.path))) should beInvalidWithError(new CustomValidationError("", "a.b.c"))
  }

  test("Error can be mapped with a dispatcher") {
    implicit val mapEmptyString = ErrorMap.dispatch {
      case e: IsEmptyStringError => new CustomValidationError(e.value, "a.b.c", e.args, e.path)
    }
    notBlank("") should beInvalidWithError(new CustomValidationError("", "a.b.c"))
  }

  test("Error can be mapped with a dispatcher fallback") {
    implicit val mapEmptyString = ErrorMap.dispatch {
      case e: CustomValidationError => new CustomValidationError(e.value, "a.b.c", e.args, e.path)
    }
    notBlank("") should beInvalidWithError(new IsEmptyStringError())
  }

  val ageValidator = Validator.template[Int] { age =>
    val mapToAgeError = ErrorMap
    age is_>= 18
    ???
  }
}
