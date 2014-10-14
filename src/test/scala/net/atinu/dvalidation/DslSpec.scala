package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationMatcher
import org.scalatest.{ Matchers, FunSuite }

class DslSpec extends FunSuite with Matchers with ValidationMatcher {

  import Validator._

  test("String is empty") {
    notEmpty("") should beInvalidWithError(new IsEmptyStringError(""))
  }

  test("String is not empty") {
    notEmpty("s") should beValidResult("s")
  }

  test("validate any value") {
    ensure(1)("error.dvalidation.isequal", 1)(_ == 1) should beValidResult(1)
  }

  test("validate any value 2") {
    ensure(1)("error.dvalidation.isequal", 2)(_ == 2) should beInvalidWithError(new CustomValidationError(1, "error.dvalidation.isequal", Seq("2")))
  }

  test("List is empty") {
    hasElements(List.empty[String]) should beInvalidWithError(new IsEmptySeqError())
  }

  test("List is not empty") {
    val a: DValidation[List[Int]] = hasElements(List(1, 2))
    a should beValidResult(List(1, 2))
  }

  test("Option is empty") {
    isSome(None.asInstanceOf[Option[String]]) should beInvalidWithError(new IsNoneError())
  }

  test("Option is not empty") {
    val a: DValidation[Option[Int]] = isSome(Some(1))
    a should beValidResult(Some(1))
  }

  test("Option can be seen as valid validation") {
    Some(1).asValidation should beValidResult(1)
  }

  test("Option can be seen as invalid validation") {
    val opt: Option[Int] = None
    opt.asValidation should beInvalidWithError(new IsNoneError())
  }

  test("Try success can be seen as valid validation") {
    scala.util.Success(1).asValidation should beValidResult(1)
  }

  test("Try failure can be seen as invalid validation") {
    val exception = new IllegalArgumentException
    scala.util.Failure(exception).asValidation should beInvalidWithError(new IsTryFailureError(exception))
  }

  def isEqual[T](valueCheck: T, valueExcept: T) =
    ensure(valueCheck)("error.dvalidation.isequal", valueExcept)(a => a == valueExcept)

  test("define a validation chain") {
    val a = 1
    val v1 = isEqual(a, 1)
    val v2 = isEqual(a, 2)

    val resV = v1.findSuccess(v2)

    resV should beValid
  }

  test("define a validation chain2") {
    val a = 3
    val v1 = isEqual(a, 1)
    val v2 = isEqual(a, 2)

    val resV = v1.findSuccess(v2)

    resV should beInvalid
  }

  test("define a specific validation chain") {
    val a = 1
    val v1 = isEqual(a, 1)
    val v2 = isEqual(a, 2)

    val resV = v1.isValidOr(v2)

    resV should beValid
  }

  test("define a specific validation chain2") {
    val a = 3
    val v1 = isEqual(a, 1)
    val v2 = isEqual(a, 2)

    val resV = v1 isValidOr v2

    resV should beInvalid
  }

  test("do a custom validation") {
    val a = if ("a" == "a") "a".valid else invalid("a", "error.notA")
    a should beValid
  }

  test("do a custom validation 2") {
    val a = Validator.validate("a")(_ == "a")(error = new CustomValidationError("a", "error.notA"))
    a should beValid
  }

  case class VTest(a: Int, b: String, c: Option[String])

  test("validate case class") {
    val vtest = VTest(1, "sdf", Some("a"))
    val res: DValidation[VTest] = vtest.validateWith(
      ensure(vtest.a)("should be 1", 1)(_ == 1),
      notEmpty(vtest.b)
    )
    res should beValidResult(vtest)
  }

  test("validate case class 2") {
    val vtest = VTest(1, "", Some("a"))
    val res = vtest.validateWith(
      ensure(vtest.a)("validation.equal", 2)(_ == 2),
      notEmpty(vtest.b)
    )
    res should beInvalidWithErrors(
      new CustomValidationError(1, "validation.equal", Seq("2")),
      new IsEmptyStringError()
    )
  }

  case class VTestNested(value: Int, nest: VTest)

  test("validate case class for attribute") {
    val vtest = VTest(1, "", None)
    val validateWith = vtest.validateWith(
      isEqual(vtest.a, 2) forAttribute 'a,
      notEmpty(vtest.b) forAttribute 'b,
      isSome(vtest.c) forAttribute 'c
    )
    validateWith should beInvalidWithErrors(
      new CustomValidationError(1, "error.dvalidation.isequal", Seq("2"), "/a"),
      new IsEmptyStringError("/b"),
      new IsNoneError("/c")
    )

    val vTestNested = VTestNested(5, vtest)
    val resNest = vTestNested.validateWith(
      isEqual(vTestNested.value, 1).forAttribute('value),
      validateWith.forAttribute('nest)
    )
    resNest should beInvalidWithErrors(
      CustomValidationError(5, "error.dvalidation.isequal", args = "1").nestPath("value"),
      CustomValidationError(1, "error.dvalidation.isequal", args = "2").nestPath("nest/a"),
      new IsEmptyStringError("/nest/b"),
      new IsNoneError("/nest/c")
    )
  }

  case class VTestSeq(value: Int, tests: Seq[VTest])

  test("validate a seq") {
    val vtest = VTest(1, "", None)
    val vtest2 = VTest(2, "", Some("d"))
    val vtseq = VTestSeq(1, Seq(vtest, vtest2))

    val vtestValidator: DValidator[VTest] = Validator.template[VTest] { value =>
      value.validateWith(
        isEqual(value.a, 2) forAttribute 'a,
        notEmpty(value.b) forAttribute 'b,
        isSome(value.c) forAttribute 'c
      )
    }

    val res = vtseq.validateWith(
      isEqual(vtseq.value, 2) forAttribute 'value
    ).withValidations(
        validSequence(vtseq.tests, vtestValidator) forAttribute 'tests
      )

    res should beInvalidWithErrors(
      CustomValidationError(1, "error.dvalidation.isequal", args = "2").nestPath("value"),
      CustomValidationError(1, "error.dvalidation.isequal", args = "2").nestPath("tests/[0]/a"),
      new IsEmptyStringError("/tests/[0]/b"),
      new IsNoneError("/tests/[0]/c"),
      new IsEmptyStringError("/tests/[1]/b")
    )
  }
}
