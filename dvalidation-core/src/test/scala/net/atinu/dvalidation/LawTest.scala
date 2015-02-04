package net.atinu.dvalidation

import net.atinu.dvalidation.errors._
import org.scalacheck.{ Arbitrary, Gen, Properties }

import scalaz._
import scalaz.scalacheck.ScalazProperties._

object LawTest extends Properties("DomainErrors") {

  def deGen: Gen[DomainError] = for {
    value <- Gen.alphaStr
    key <- Gen.alphaStr
    args <- Gen.listOf(Gen.alphaStr)
    errors <- Gen.oneOf(
      new CustomValidationError(value, key, args),
      new IsEmptyStringError(),
      new IsEmptySeqError())
  } yield errors

  def desGen: Gen[DomainErrors] = for {
    amount <- Gen.chooseNum(1, 10)
    elems <- Gen.listOfN(amount, deGen)
  } yield DomainErrors.apply(elems.head, elems.tail: _*)

  implicit val desArb = Arbitrary(desGen)

  semigroup.laws[DomainErrors].check

  equal.laws[DomainErrors].check

}
