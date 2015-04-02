package net.atinu.dvalidation

import net.atinu.dvalidation.errors.IsEmptyStringError
import net.atinu.dvalidation.util.ValidationSuite
import net.atinu.dvalidation._
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation.scopes._

class ScopeSpec extends ValidationSuite {

  case class CTest(a: String, b: String, c: String)
  val emptyVal = new CTest("", "", "")

  test("a scoped validation can apply validations in a named scope") {
    import SymbolScope._
    emptyVal.validateScoped('foo)(
      inScope('foo)(notBlank(emptyVal.a) forAttribute 'a),
      inScope('foo2)(notBlank(emptyVal.b) forAttribute 'b),
      inScope('foo2)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithError(new IsEmptyStringError(Path.wrap("/a")))
  }

  test("a scoped validation can apply validations in a symbol named scope") {
    import SymbolScope._
    emptyVal.validateScoped('foo2)(
      inScope('foo)(notBlank(emptyVal.a) forAttribute 'a),
      inScope('foo2)(notBlank(emptyVal.b) forAttribute 'b),
      inScope('foo2)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can apply multible validations in one scope") {
    import SymbolScope._
    emptyVal.validateScoped('foo2)(
      inScope('foo)(notBlank(emptyVal.a) forAttribute 'a),
      allInScope('foo2)(List(
        notBlank(emptyVal.b) forAttribute 'b,
        notBlank(emptyVal.c) forAttribute 'c))
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can have multible validations for one scope") {
    import SymbolScope._
    emptyVal.validateScoped('too)(
      inScope('foo, 'too)(notBlank(emptyVal.a) forAttribute 'a),
      inScope('foo2)(notBlank(emptyVal.b) forAttribute 'b),
      inScope('foo2)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a"))
      )
  }

  test("a scoped validation can have validate for any of multile scopes") {
    import SymbolScope._
    emptyVal.validateAnyScope('foo, 'foo2)(
      inScope('foo)(notBlank(emptyVal.a) forAttribute 'a),
      inScope('foo1)(notBlank(emptyVal.b) forAttribute 'b),
      inScope('foo2)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can have validate for all of multile scopes") {
    import SymbolScope._
    emptyVal.validateAllScopes('foo, 'foo2)(
      inScope('foo, 'foo2, 'foo3)(notBlank(emptyVal.a) forAttribute 'a),
      inScope('foo1)(notBlank(emptyVal.b) forAttribute 'b),
      inScope('foo2)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithError(
        new IsEmptyStringError(Path.wrap("/a"))
      )
  }

  test("a custom scope can be inferred from a scalaz.Equals instance") {
    import EqualityScope._
    import scalaz.std.string._
    emptyVal.validateScoped("foo2")(
      inScope("foo")(notBlank(emptyVal.a) forAttribute 'a),
      inScope("foo2")(notBlank(emptyVal.b) forAttribute 'b),
      inScope("foo2")(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/b")),
        new IsEmptyStringError(Path.wrap("/c"))
      )
  }

  test("a scoped validation can apply validations in a hierarchical scope") {
    import Path._
    import PathScope._
    emptyVal.validateScoped("/foo".asPath)(
      inScope("/foo/a".asPath)(notBlank(emptyVal.a) forAttribute 'a),
      inScope("/foo/b".asPath)(notBlank(emptyVal.b) forAttribute 'b),
      inScope("/bar/c".asPath)(notBlank(emptyVal.c) forAttribute 'c)
    ) should beInvalidWithErrors(
        new IsEmptyStringError(Path.wrap("/a")),
        new IsEmptyStringError(Path.wrap("/b"))
      )
  }

  test("a hierarchical scope treads parent/child scopes as matches") {
    import Path._
    val hs = PathScope.pathScope
    hs.matches("/foo".asPath, "/foo/bar".asPath) should equal(true)
    hs.matches("/foo/bar".asPath, "/foo".asPath) should equal(false)
    hs.matches("/foo".asPath, "/bar/c".asPath) should equal(false)
  }

}
