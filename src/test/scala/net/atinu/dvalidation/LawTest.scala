package net.atinu.dvalidation

import org.scalacheck.{ Arbitrary, Gen, Properties }

import scalaz._
import Scalaz._
import scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalaCheckBinding._
import net.atinu.dvalidation._

object LawTest extends Properties("DomainErrors") {

  def deGen: Gen[DomainError] = for {
    value <- Gen.alphaStr
    key <- Gen.alphaStr
    args <- Gen.listOf(Gen.alphaStr)
  } yield new CustomValidationError(value, key, args)

  def desGen: Gen[DomainErrors] = for {
    amount <- Gen.chooseNum(1, 10)
    elems <- Gen.listOfN(amount, deGen)
  } yield DomainErrors.apply(elems.head, elems.tail: _*)

  implicit val desArb = Arbitrary(desGen)

  semigroup.laws[DomainErrors].check

}
