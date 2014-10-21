package net.atinu.dvalidation

import scalaz._

sealed trait Path

object Path {

  import scalaz.Id._

  type PathString = String @@ Path

  val SingleSlash = wrap("/")

  implicit class StringToPath(val v: String) extends AnyVal {
    def asPath: PathString = Path.wrap(v)
  }

  private lazy val r = """(/{1}+)|((/{1}+)([^/]+/{1}+|[^/]+)*?[^/]+)""".r.pattern

  def isValidPath(path: String): Boolean = r.matcher(path).matches()

  def wrap(path: String): PathString =
    if (isValidPath(path)) wrapInternal(path)
    else throw new IllegalArgumentException(s"$path is not a valid path")

  private[dvalidation] def wrapInternal(path: String): PathString = Tag[String, Path](path)

  def unwrap(path: PathString): String = Tag.unsubst[String, Id, Path](path)
}
