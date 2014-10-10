package com.atinu.dvalidation

import com.atinu.dvalidation.util.ValidationMatcher
import org.scalatest.{Matchers, FunSuite}

class DslSpec extends FunSuite with Matchers with ValidationMatcher {

  import Validator._

  test("String is empty") {
    notEmpty("") should beInvalid
  }

  test("String is not empty") {
    notEmpty("s") should beValid
  }

  test("validate any value") {
    ensure(1)("error.dvalidation.isequal", 1)(_ == 1) should beValid
  }

  test("validate any value 2") {
    ensure(1)("error.dvalidation.isequal", 2)( _ == 2) should beInvalid
  }

  test("List is empty") {
    hasElements(List.empty[String]) should beInvalid
  }

  test("List is not empty") {
    val a: DValidation[List[Int]] = hasElements(List(1,2))
    a should beValid
  }

  test("Option is empty") {
    isSome(None.asInstanceOf[Option[String]]) should beInvalid
  }

  test("Option is not empty") {
    val a: DValidation[Option[Int]] = isSome(Some(1))
    a should beValid
  }

  test("Option should be validated") {
    val a = isSome(Some(1)).ensure("error")(_.get == 1)
    a should beValid
  }

  test("Option should be validated 2") {
    val a = isSome(Some(1)).ensure("error")(_.get == 2)
    a should beInvalid
  }

  test("Option can be seen as valid validation") {
    Some(1).asValidation should beValid
  }

  test("Option can be seen as invalid validation") {
    val opt: Option[Int] = None
    opt.asValidation should beInvalid
  }

  def isEqual[T](valueExcept:T, valueCheck: T) =
    ensure(valueCheck)("error.dvalidation.isequal", valueExcept)( a => a == valueExcept)

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
    val a = if("a" == "a") "a".valid else invalid("a", "error.notA")
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
    res should beValid
    res.toOption.get should equal(vtest)
  }

  test("validate case class 2") {
    val vtest = VTest(1, "", Some("a"))
    val res = vtest.validateWith(
      ensure(vtest.a)("should be 2", 2)(_ == 2),
      notEmpty(vtest.b)
    )
    //println(res)
    res should beInvalid
  }

  case class VTestNested(value: Int, nest: VTest)

  test("validate case class for attribute") {
    val vtest = VTest(1, "", None)
    val validateWith = vtest.validateWith(
      isEqual(vtest.a, 2) forAttribute 'a,
      notEmpty(vtest.b) forAttribute 'b,
      isSome(vtest.c) forAttribute 'c
    )
    //println(validateWith.errorView.get.prettyPrint)
    validateWith should beInvalid

    val vTestNested = VTestNested(5, vtest)
    val resNest = vTestNested.validateWith(
      isEqual(vTestNested.value, 1).forAttribute('value),
      validateWith.forAttribute('nest)
    )
    //println(resNest.errorView.get.prettyPrint)
  }

  case class VTestSeq(value: Int, tests: Seq[VTest])

  test("validate a seq") {
    val vtest = VTest(1, "", None)
    val vtest2 = VTest(2, "", Some("d"))
    val vtseq = VTestSeq(1, Seq(vtest, vtest2))

    val vtestValidator: DValidator[VTest] = Validator.template[VTest] { value =>
      value.validateWith(
        isEqual(2, value.a) forAttribute 'a,
        notEmpty(value.b) forAttribute 'b,
        isSome(value.c) forAttribute 'c
      )
    }

    val res = vtseq.validateWith(
      isEqual(2, vtseq.value) forAttribute 'value
    ).withValidations(
      validSequence(vtseq.tests, vtestValidator) forAttribute 'tests
    )

    println(res.errorView.get.prettyPrint)
  }
}
