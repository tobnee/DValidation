package net.atinu.dvalidation

import net.atinu.dvalidation.errors.{ IsZeroError, IsEmptyError }
import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation.validator.GenericValidator._

class GenericValidationSpec extends ValidationSuite {

  test("Validate if not Monoid zero") {
    import scalaz.std.anyVal._
    notZero(1) === scalaz.Success(1)
  }

  test("Validate if not Monoid zero - error") {
    import scalaz.std.anyVal._
    notZero(0) should beInvalidWithError(new IsZeroError(0))
  }

  test("not empty - default string") {
    import scalaz.std.string._
    nonEmptyGeneric("") should beInvalidWithError(new IsEmptyError(""))
  }

  test("not empty - default list") {
    import scalaz.std.list._
    nonEmptyGeneric(List.empty[Int]) should beInvalidWithError(new IsEmptyError(List.empty[Int]))
  }

  test("not empty - default map") {
    import scalaz.std.map._
    nonEmptyGeneric(Map.empty[String, Int]) should beInvalidWithError(new IsEmptyError(Map.empty[String, Int]))
  }

  test("not empty - default option") {
    import scalaz.std.option._
    nonEmptyGeneric(None.asInstanceOf[Option[String]]) should beInvalidWithError(new IsEmptyError(None))
  }

}
