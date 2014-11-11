package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.errors._

class PathSpec extends ValidationSuite {

  case class VTest(a: Int, b: String, c: Option[String])

  def isEqual[T](valueCheck: T, valueExcept: T) =
    ensure(valueCheck)("error.dvalidation.isequal", valueExcept)(a => a == valueExcept)

  test("validate paths") {
    Path.isValidPath("") should be(false)
    Path.isValidPath("/sd/") should be(false)
    Path.isValidPath("/") should be(true)
    Path.isValidPath("/tests/[1]/b") should be(true)
    Path.isValidPath("/tests") should be(true)
  }

  test("build paths") {
    val vtest = VTest(1, "", None)
    vtest.validateWith(isEqual(vtest.a, 2) forAttribute 'a forAttribute 'b) should beInvalidWithError(
      CustomValidationError(1, "error.dvalidation.isequal", args = "2")
        .nestAttribute('a).nestAttribute('b))
  }

}
