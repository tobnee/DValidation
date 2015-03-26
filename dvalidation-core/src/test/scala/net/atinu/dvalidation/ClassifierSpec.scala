package net.atinu.dvalidation

import net.atinu.dvalidation.errors.IsEmptyStringError
import net.atinu.dvalidation.util.ValidationSuite
import Validator._

class ClassifierSpec extends ValidationSuite {
  import scalaz.std.anyVal._

  test("foo") {
    val a = ""
    a.validateWith(
      notBlank(""),
      isGreaterThan(a.length, 0)
    ) should beInvalidWithError(new IsEmptyStringError())
  }

}
