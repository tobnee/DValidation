package net.atinu.dvalidation.util

import net.atinu.dvalidation.{ Path, DomainErrors }
import net.atinu.dvalidation.errors.{ IsNoneError, IsEmptyStringError }
import org.scalatest.{ Matchers, FunSuite }

class ValidationMatcherTest extends FunSuite with Matchers {
  import Path._

  test("Show missing validations") {
    val e1 = new IsNoneError("/tests/[0]/c".asPath)
    val e2 = new IsEmptyStringError("/tests/[1]/b".asPath)
    val exp = DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      e1, e2
    )
    val got = DomainErrors.withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath))

    val res = ValidationMatcher.compareErrors(exp, got)
    res should not be 'OK
    res.less should contain only (e1, e2)
  }

  test("Show too many validations") {
    val e1 = new IsNoneError("/tests/[0]/c".asPath)
    val e2 = new IsEmptyStringError("/tests/[1]/b".asPath)
    val exp = DomainErrors.withErrors(
      new IsEmptyStringError("/tests/[0]/b".asPath),
      e1, e2
    )
    val got = DomainErrors.withSingleError(new IsEmptyStringError("/tests/[0]/b".asPath))

    val res = ValidationMatcher.compareErrors(got, exp)
    res should not be 'OK
    res.more should contain only (e1, e2)
  }

  test("Show validation differences") {
    val e1 = new IsNoneError("/tests/[0]/c".asPath)
    val e2 = new IsEmptyStringError("/tests/[1]/b".asPath)
    val g1 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val g2 = new IsEmptyStringError("/tests/[0]/b".asPath)
    val exp = DomainErrors.withErrors(e1, e2)
    val got = DomainErrors.withErrors(g1, g2)

    val res = ValidationMatcher.compareErrors(exp, got)
    println(res.toString)
    res should not be 'OK
    res.diff should contain only (e1 -> g1, e2 -> g2)
  }
}
