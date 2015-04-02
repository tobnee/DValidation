package net.atinu.dvalidation

import net.atinu.dvalidation.errors.IsEmptyStringError
import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation._
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.scopes._

class ScopeSpec extends ValidationSuite {

  case class CTest(a: String, b: String, c: String)

  test("a scoped validation can apply validations in a named scope") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateScoped('foo)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithError(new IsEmptyStringError(Path.wrap("/a")))
  }

  test("a scoped validation can apply validations in a symbol named scope") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateScoped('foo2)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can apply multible validations in one scope") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateScoped('foo2)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      allInScope('foo2)(List(
        notBlank(value.b) forAttribute 'b,
        notBlank(value.c) forAttribute 'c))
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can have multible validations for one scope") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateScoped('too)(
      inScope('foo, 'too)(notBlank(value.a) forAttribute 'a),
      inScope('foo2)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a"))
      )
  }

  test("a scoped validation can have validate for any of multile scopes") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateAnyScope('foo, 'foo2)(
      inScope('foo)(notBlank(value.a) forAttribute 'a),
      inScope('foo1)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can have validate for all of multile scopes") {
    val value = new CTest("", "", "")
    import SymbolScope._
    value.validateAllScopes('foo, 'foo2)(
      inScope('foo, 'foo2, 'foo3)(notBlank(value.a) forAttribute 'a),
      inScope('foo1)(notBlank(value.b) forAttribute 'b),
      inScope('foo2)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithError(
        new IsEmptyStringError(Path.wrap("/a"))
      )
  }

  test("a custom scope can be inferred from a scalaz.Equals instance") {
    val value = new CTest("", "", "")
    import EqualityScope._
    import scalaz.std.string._
    value.validateScoped("foo2")(
      inScope("foo")(notBlank(value.a) forAttribute 'a),
      inScope("foo2")(notBlank(value.b) forAttribute 'b),
      inScope("foo2")(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can apply validations in a hierarchical scope") {
    val value = new CTest("", "", "")
    import Path._
    import PathScope._
    value.validateScoped("/foo".asPath)(
      inScope("/foo/a".asPath)(notBlank(value.a) forAttribute 'a),
      inScope("/foo/b".asPath)(notBlank(value.b) forAttribute 'b),
      inScope("/bar/c".asPath)(notBlank(value.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a")),
        new IsEmptyStringError(Path.wrap("/b"))
      )
  }

  test("a hierarchical scope treads parent/child scopes as matches") {
    import PathScope._
    import Path._
    val hs = implicitly[Scope[PathString]]
    hs.matches("/foo".asPath, "/foo/bar".asPath) should equal(true)
    hs.matches("/foo/bar".asPath, "/foo".asPath) should equal(false)
    hs.matches("/foo".asPath, "/bar/c".asPath) should equal(false)
  }

}
