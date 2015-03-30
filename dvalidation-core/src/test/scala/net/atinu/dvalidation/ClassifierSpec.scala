package net.atinu.dvalidation

import net.atinu.dvalidation.errors.IsEmptyStringError
import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation._
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.ScopedValidations._

class ClassifierSpec extends ValidationSuite {

  case class CTest(a: String, b: String, c: String)

  test("a classifer can apply validations in scope") {
    val value = new CTest("", "", "")
    import ScopedValidations.SymbolScope._
    value.validateCategory('foo)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithError(new IsEmptyStringError(Path.wrap("/a")))
  }

  test("a classifer can apply validations in scope 2") {
    val value = new CTest("", "", "")
    import ScopedValidations.SymbolScope._
    value.validateCategory('foo2)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a classifer can apply multible validations in one scope") {
    val value = new CTest("", "", "")
    import ScopedValidations.SymbolScope._
    value.validateCategory('foo2)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      allInScope('foo2)(List(
        notBlank(value.b) forAttribute 'b,
        notBlank(value.c) forAttribute 'c))
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can have multible scopes") {
    val value = new CTest("", "", "")
    import ScopedValidations.SymbolScope._
    value.validateCategory('too)(
      inScope('foo, 'too)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a"))
      )
  }

  test("a custom scope can be inferred from a scalaz.Equals instance") {
    val value = new CTest("", "", "")
    import ScopedValidations.EqualityScope._
    import scalaz.std.string._
    value.validateCategory("foo2")(
      inCustomScope("foo")(notBlank(value.a) forAttribute 'a),
      inCustomScope("foo2")(notBlank(value.b) forAttribute 'b),
      inCustomScope("foo2")(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a classifer can apply validations in a hierarchical scope") {
    val value = new CTest("", "", "")
    import Path._
    import ScopedValidations.PathScope._
    value.validateCategory("/foo".asPath)(
      inCustomScope("/foo/a".asPath)(notBlank(value.a) forAttribute 'a),
      inCustomScope("/foo/b".asPath)(notBlank(value.b) forAttribute 'b),
      inCustomScope("/bar/c".asPath)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a")),
        new IsEmptyStringError(Path.wrap("/b"))
      )
  }

  test("a hierarchical scope") {
    import ScopedValidations.PathScope._
    import Path._
    val hs = implicitly[Scope[PathString]]
    hs.matches("/foo".asPath, "/foo/bar".asPath) should equal(true)
    hs.matches("/foo/bar".asPath, "/foo".asPath) should equal(false)
    hs.matches("/foo".asPath, "/bar/c".asPath) should equal(false)
  }

}
