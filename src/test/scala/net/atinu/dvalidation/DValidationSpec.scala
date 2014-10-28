package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationSuite

class DValidationSpec extends ValidationSuite {

  import net.atinu.dvalidation.Path._
  import net.atinu.dvalidation.Validator._

  test("String is blank") {
    notBlank("") should beInvalidWithError(new IsEmptyStringError())
  }

  test("Error can be mapped") {
    implicit def errorMap = new ErrorMap[IsEmptyStringError] {
      def apply(v1: IsEmptyStringError): DomainError = DomainError.wrapWithKey(v1, "a.b.c")
    }
    notBlank("")(errorMap) should beInvalidWithErrorProps("", "a.b.c", "/")
  }

  test("String with whitespace is blank") {
    notBlank(" ") should beInvalidWithError(new IsEmptyStringError())
  }

  test("String with whitespace is not blank") {
    notBlank(" ", trimWhitespace = false) should beValidResult(" ")
  }

  test("String is not blank") {
    notBlank("s") should beValidResult("s")
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

  test("Validate is equal") {
    isEqual(2, 2) should beValidResult(2)
    (2 is_== 2) should beValidResult(2)
  }

  test("validate is equal - error") {
    isEqual(2, 3) should beInvalidWithError(new IsNotEqualError(2, 3))
  }

  test("Validate is equal strict") {
    import scalaz.std.anyVal._
    isEqualStrict(2, 2) should beValidResult(2)
    (2 is_=== 2) should beValidResult(2)
  }

  test("validate is equal strict - error") {
    import scalaz.std.anyVal._
    isEqualStrict(2, 3) should beInvalidWithError(new IsNotEqualError(2, 3))
    (2 is_=== 3) should beInvalidWithError(new IsNotEqualError(2, 3))
  }

  test("Validate a > b") {
    isGreaterThan(3, 2) should beValidResult(3)
    3 is_> 2 should beValidResult(3)
  }

  test("Validate a > b - error") {
    isGreaterThan(2, 3) should beInvalidWithError(new IsNotGreaterThenError(3, 2, false))
  }

  test("Validate a >= b") {
    isGreaterThan(3, 3, isInclusive = true) should beValidResult(3)
    (3 is_>= 3) should beValidResult(3)
  }

  test("Validate a >= b - error") {
    isGreaterThan(2, 3, isInclusive = true) should beInvalidWithError(new IsNotGreaterThenError(3, 2, true))
  }

  test("Validate a < b") {
    isSmallerThan(2, 3) should beValidResult(2)
    2 is_< 3 should beValidResult(2)
  }

  test("Validate a < b - error") {
    isSmallerThan(3, 2) should beInvalidWithError(new IsNotLowerThenError(2, 3, false))
  }

  test("Validate a <= b") {
    isSmallerThan(3, 3, isInclusive = true) should beValidResult(3)
    (3 is_<= 3) should beValidResult(3)
  }

  test("Validate a <= b - error") {
    isSmallerThan(3, 2, isInclusive = true) should beInvalidWithError(new IsNotLowerThenError(2, 3, true))
  }

  test("Validate a < b < c") {
    isInRange(4, min = 1, max = 5) should beValidResult(4)
  }

  test("Validate a < b < c - error high") {
    isInRange(6, min = 1, max = 5) should beInvalidWithError(new IsNotLowerThenError(5, 6, false))
  }

  test("Validate a < b < c - error low") {
    isInRange(0, min = 1, max = 5) should beInvalidWithError(new IsNotGreaterThenError(1, 0, false))
  }

  test("isInRange should only work on valid ranges") {
    intercept[IllegalArgumentException] {
      isInRange(0, min = 8, max = 7)
    }
    intercept[IllegalArgumentException] {
      isInRange(0, min = 8, max = 8)
    }
  }

  test("Validate if not Monoid zero") {
    import scalaz.std.anyVal._
    notZero(1) should beValidResult(1)
  }

  test("Validate if not Monoid zero - error") {
    import scalaz.std.anyVal._
    notZero(0) should beInvalidWithError(new IsZeroError(0))
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
      notBlank(vtest.b)
    )
    res should beValidResult(vtest)
  }

  test("validate case class 2") {
    val vtest = VTest(1, "", Some("a"))
    val res = vtest.validateWith(
      ensure(vtest.a)("validation.equal", 2)(_ == 2),
      notBlank(vtest.b)
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
      (vtest.a is_== 2) forAttribute 'a,
      notBlank(vtest.b) forAttribute 'b,
      isSome(vtest.c) forAttribute 'c
    )
    validateWith should beInvalidWithErrors(
      new IsNotEqualError(1, 2).nestAttribute('a),
      new IsEmptyStringError("/b".asPath),
      new IsNoneError("/c".asPath)
    )

    val vTestNested = VTestNested(5, vtest)
    val resNest = vTestNested.validateWith(
      isEqual(vTestNested.value, 1).forAttribute('value),
      validateWith.forAttribute('nest)
    )
    resNest should beInvalidWithErrors(
      new IsNotEqualError(5, 1)
        .nestAttribute('value),
      new IsNotEqualError(1, 2)
        .nestAttribute('a).nestAttribute('nest),
      new IsEmptyStringError("/nest/b".asPath),
      new IsNoneError("/nest/c".asPath)
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
        notBlank(value.b) forAttribute 'b,
        isSome(value.c) forAttribute 'c
      )
    }

    val res = vtseq.validateWith(
      isEqual(vtseq.value, 2) forAttribute 'value
    ).withValidations(
        validSequence(vtseq.tests, vtestValidator) forAttribute 'tests
      )

    res should beInvalidWithErrors(
      new IsNotEqualError(1, 2).nestAttribute('value),
      new IsNotEqualError(1, 2).nest("/tests/[0]/a".asPath),
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    )
  }
}
