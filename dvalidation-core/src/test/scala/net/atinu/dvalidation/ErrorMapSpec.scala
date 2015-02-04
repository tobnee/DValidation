package net.atinu.dvalidation

import java.time.LocalDateTime

import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation.errors._

import scalaz.Ordering

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

    notBlank("")(mapEmptyString) should beInvalidWithError(new CustomValidationError("", "a.b.c"))
  }

  test("Error can be mapped with lifted function") {
    notBlank("")(v1 => new CustomValidationError(v1.value, "a.b.c", v1.args, v1.path)) should beInvalidWithError(new CustomValidationError("", "a.b.c"))
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

  val lDtOrder = scalaz.Order.order[LocalDateTime]((a, b) => if (a.isBefore(b)) Ordering.LT else Ordering.GT)
  val toInPastError = ErrorMap.mapKey[IsNotLowerThenError]("dvalidaiton.inPast")

  def inPast(dt: java.time.LocalDateTime) = Validator.isSmallerThan(dt, LocalDateTime.now())(lDtOrder, toInPastError)

  test("Error mapping can define a custom validator") {
    inPast(LocalDateTime.now().minusDays(1)) should beValid
    val dateTime = LocalDateTime.now().plusDays(1)
    val dValidation = inPast(dateTime)
    dValidation should beInvalid
  }

  test("Error can be mapped with a bound dispatch") {
    import scalaz.std.list._
    implicit def a = ErrorMap.dispatchFor[WrongSizeError] {
      case e: IsToBigError => CustomValidationError.withKey(e, "foo")
    }
    hasSize(List(1, 2, 3), max = 2) should beInvalidWithErrorProps(3, "foo", "/", "2")
  }
}
