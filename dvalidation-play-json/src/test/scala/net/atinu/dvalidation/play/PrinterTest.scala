package net.atinu.dvalidation.play

import _root_.play.api.libs.json.{ JsString, Json }
import net.atinu.dvalidation.Validator._
import net.atinu.dvalidation._
import org.scalatest.{ Matchers, FunSuite }

class PrinterTest extends FunSuite with Matchers {

  trait Classification

  case object StringInstrument extends Classification

  case object Keyboard extends Classification

  abstract class Instrument(val classification: Classification)

  case object BassGuitar extends Instrument(StringInstrument)

  case object Guitar extends Instrument(StringInstrument)

  case object Piano extends Instrument(Keyboard)

  case class Musician(name: String, age: Int, instruments: Seq[Instrument])

  test("hejso") {
    val max = Musician("Max Mustermann", 12, List(Piano, Guitar))

    val stringInstrumentValidator = Validator.template[Instrument](i =>
      ensure(i)(key = "error.dvalidation.stringinstrument", args = i.classification)(_.classification == StringInstrument)
    )

    val res = max.validateWith(
      notBlank(max.name) forAttribute 'name,
      ensure(max.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
      hasElements(max.instruments) forAttribute 'instruments
    ).withValidations(
        validSequence(max.instruments)(stringInstrumentValidator) forAttribute 'instruments
      )
    val resJson = res.fold(error => Printer.print(error), succ => JsString("ok"))
    println(resJson.toString())
  }

}
