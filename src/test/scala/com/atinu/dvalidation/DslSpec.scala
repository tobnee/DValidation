package com.atinu.dvalidation

import com.atinu.dvalidation.util.ValidationMatcher
import org.scalatest.{Matchers, FunSuite}

class DslSpec extends FunSuite with Matchers with ValidationMatcher {

  import Validator._
  import DomainErrors._

  test("String is empty") {
    notEmpty("") should beInvalid
  }

  test("String is not empty") {
    notEmpty("s") should beValid
  }

  test("validate any value") {
    ensure(1)("should be 1", _ == 1) should beValid
  }

  test("validate any value 2") {
    ensure(1)("should be 1", _ == 2) should beInvalid
  }

  test("List is empty") {
    hasElements(List.empty[String]) should beInvalid
  }

  test("List is not empty") {
    val a: DValidation[List[Int]] = hasElements(List(1,2))
    a should beValid
  }

  test("Option is empty") {
    hasElement(None.asInstanceOf[Option[String]]) should beInvalid
  }

  test("Option is not empty") {
    val a: DValidation[Option[Int]] = hasElement(Some(1).asInstanceOf[Option[Int]])
    a should beValid
  }

  test("Option should be validated") {
    val a = hasElement(Some(1).asInstanceOf[Option[Int]]).ensure("error")(_.get == 1)
    a should beValid
  }

  test("Option should be validated 2") {
    val a = hasElement(Some(1).asInstanceOf[Option[Int]]).ensure("error")(_.get == 2)
    a should beInvalid
  }

  test("define a validation chain") {
    val a = 1
    val v1 = ensure(a)("should be 1", a => a == 1)
    val v2 = ensure(a)("should be 2", a => a == 2)

    val resV = v1.findSuccess(v2)

    resV should beValid
  }

  test("define a validation chain2") {
    val a = 3
    val v1 = ensure(a)("should be 1", a => a == 1)
    val v2 = ensure(a)("should be 2", a => a == 2)

    val resV = v1.findSuccess(v2)

    resV should beInvalid
  }

  test("define a specific validation chain") {
    val a = 1
    val v1 = ensure(a)("should be 1", a => a == 1)
    val v2 = ensure(a)("should be 2", a => a == 2)

    val resV = v1.isValidOr(v2)

    resV should beValid
  }

  test("define a specific validation chain2") {
    val a = 3
    val v1 = ensure(a)("should be 1", a => a == 1)
    val v2 = ensure(a)("should be 2", a => a == 2)

    val resV = v1 isValidOr v2

    resV should beInvalid
  }

  test("do a custom validation") {
    val a = if("a" == "a") "a".valid else invalid("a", "error.notA")
    a should beValid
  }

  test("do a custom validation 2") {
    val a = DomainErrors.validate("a")(_ == "a")(error = new CustomValidationError("a", "error.notA"))
    a should beValid
  }

  case class VTest(a: Int, b: String, c: Option[String])

  test("validate case class") {
    val vtest = VTest(1, "sdf", Some("a"))
    val res: DValidation[VTest] = vtest.validateWith(
      ensure(vtest.a)("should be 1", _ == 1),
      notEmpty(vtest.b)
    )
    res should beValid
    res.toOption.get should equal(vtest)
  }

  test("validate case class 2") {
    val vtest = VTest(1, "", Some("a"))
    val res = vtest.validateWith(
      ensure(vtest.a)("should be 1", _ == 2),
      notEmpty(vtest.b)
    )
    println(res)
    res should beInvalid
  }

  case class VTestNested(value: Int, nest: VTest)

  test("validate case class for attribute") {
    val vtest = VTest(1, "", Some("a"))
    val validateWith = vtest.validateWith(
      ensure(vtest.a)("should be 1", _ == 2).forAttribute("a"),
      notEmpty(vtest.b).forAttribute("b")
    )
    val resVtest: DValidation[VTest] = validateWith.forAttribute("global")
    println(resVtest.errorView.get.prettyPrint)
    resVtest should beInvalid

    val vTestNested = VTestNested(5, vtest)
    val resNest = vTestNested.validateWith(
      ensure(vTestNested.value)("should be 1", _ == 1).forAttribute("value"),
      validateWith.forAttribute("nest")
    )
    println(resNest.errorView.get.prettyPrint)
  }
}
