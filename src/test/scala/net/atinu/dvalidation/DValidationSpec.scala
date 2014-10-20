package net.atinu.dvalidation

import net.atinu.dvalidation.util.ValidationSuite

class DValidationSpec extends ValidationSuite {

  import net.atinu.dvalidation.Path._
  import net.atinu.dvalidation.Validator._

  test("String is empty") {
    notEmpty("") should beInvalidWithError(new IsEmptyStringError())
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
      new IsNotEqualError(1, 2).nestAttribute('value),
      new IsNotEqualError(1, 2).nest("/tests/[0]/a".asPath),
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    )
  }

  test("DomainErrors can be filtered by type") {
    DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      new IsNoneError("/tests/[0]/c".asPath),
      new IsEmptyStringError("/tests/[1]/b".asPath)
    ).errorsOfType[IsNoneError] should contain(new IsNoneError("/tests/[0]/c".asPath))
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

}
