package net.atinu.dvalidation.validators

import java.time.{ Period, LocalDate }

import net.atinu.dvalidation.util.ValidationSuite

class JavaTimeValidatorSpec extends ValidationSuite {

  test("Validate inPast") {
    val res = JavaTimeValidator.inPast(LocalDate.now().minusDays(1))
    res should beValid
  }

  test("Validate inPast - error") {
    val days = LocalDate.now().plusDays(1)
    val res = JavaTimeValidator.inPast(days)
    res should beInvalid
  }

  test("Validate inPast with min bound") {
    val res = JavaTimeValidator.inPast(LocalDate.now().minusDays(2), atLeast = Period.ofDays(1))
    res should beValid
  }

  test("Validate inPast with min bound - error") {
    val days = LocalDate.now().minusDays(1)
    val res = JavaTimeValidator.inPast(days, atLeast = Period.ofDays(5))
    res should beInvalid
  }

  test("Validate inPast with max bound") {
    val res = JavaTimeValidator.inPast(LocalDate.now().minusDays(2), atMost = Period.ofDays(3))
    res should beValid
  }

  test("Validate inPast with max bound - error") {
    val days = LocalDate.now().minusDays(5)
    val res = JavaTimeValidator.inPast(days, atMost = Period.ofDays(3))
    res should beInvalid
  }

}
