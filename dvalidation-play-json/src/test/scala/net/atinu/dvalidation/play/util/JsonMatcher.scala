package net.atinu.dvalidation.play.util

import org.scalatest.matchers.{ MatchResult, Matcher }
import play.api.libs.json.{ Reads, JsObject }

trait JsonMatcher {

  def containKeyValue[T: Reads](kv: (String, T)) = new KeyValueMatcher(kv)

  def containKeyValues(k: String, v: String*) = new KeyValuesMatcher(k, v.toSeq)

  def haveNoKey(k: String) = new NoKeyValuesMatcher(k)

}

class KeyValueMatcher[T: Reads](expect: (String, T)) extends Matcher[JsObject] {

  def apply(left: JsObject): MatchResult = {
    val (k, v) = expect
    val res = (left \ k).asOpt[T].exists(_ == v)
    MatchResult(res, s"$left does not contain the key: $k // value: $v", "works")
  }
}

class KeyValuesMatcher(k: String, v: Seq[String]) extends Matcher[JsObject] {

  def apply(left: JsObject): MatchResult = {
    val res = (left \ k).asOpt[Seq[String]].exists(_ == v)
    MatchResult(res, s"$left does not contain the key: $k // value $v", "works")
  }
}

class NoKeyValuesMatcher(k: String) extends Matcher[JsObject] {

  def apply(left: JsObject): MatchResult = {
    val res = (left \ k).asOpt[Seq[String]].isEmpty
    MatchResult(res, s"$left has key '$k'", "works")
  }
}
