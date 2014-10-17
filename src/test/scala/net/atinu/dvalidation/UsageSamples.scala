package net.atinu.dvalidation

import net.atinu.dvalidation.Validator._

object UsageSamples extends App {

  trait Classification

  case object StringInstrument extends Classification

  case object Keyboard extends Classification

  abstract class Instrument(val classification: Classification)

  case object BassGuitar extends Instrument(StringInstrument)

  case object Guitar extends Instrument(StringInstrument)

  case object Piano extends Instrument(Keyboard)

  case class Musician(name: String, age: Int, instruments: Seq[Instrument])

  // Ad-Hoc validation for a case class
  val mikael = Musician("Mikael Åkerfeldt", 40, List(Guitar, BassGuitar))
  val martin = Musician("Martin Mendez", 17, List(BassGuitar))

  val res: DValidation[Musician] = mikael.validateWith(
    notEmpty(mikael.name) forAttribute 'name,
    ensure(mikael.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
    hasElements(mikael.instruments) forAttribute 'instruments
  )
  // => Success(User(Mikael Åkerfeldt,40))

  // Validation Templates
  val musicianValidator: DValidator[Musician] = Validator.template[Musician] { musician =>
    musician.validateWith(
      notEmpty(musician.name) forAttribute 'name,
      ensure(musician.age)(key = "error.dvalidation.legalage", args = 18)(_ > 18) forAttribute 'age,
      hasElements(musician.instruments) forAttribute 'instruments
    )
  }
  musicianValidator(mikael)
  // => Success(User(Mikael Åkerfeldt,40))
  musicianValidator(martin)
  // => Failure(DomainError(path: /age, value: 17, msgKey: error.dvalidation.legalage, args: 18))

  // Sequence Validation
  val max = Musician("Max Mustermann", 29, List(Piano, Guitar))

  val stringInstrumentValidator = Validator.template[Instrument](i =>
    ensure(i)(key = "error.dvalidation.stringinstrument", args = i.classification)(_.classification == StringInstrument)
  )

  max.validateWith(
    notEmpty(max.name) forAttribute 'name,
    ensure(max.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
    hasElements(max.instruments) forAttribute 'instruments
  ).withValidations(
      validSequence(max.instruments, stringInstrumentValidator) forAttribute 'instruments
    )
  // => Failure(DomainError(path: /instruments/[0], value: Piano, msgKey: error.dvalidation.stringinstrument, args: Keyboard))

  // applicative validation
  val musicianValidatorApplicative: DValidator[Musician] = Validator.template[Musician] { musician =>
    import scalaz.Scalaz._

    val stringInstrument = validSequence(musician.instruments, stringInstrumentValidator).collapse
    val atLeastOneString = stringInstrument.flatMap(value => hasElements(value))
    val legalAge = ensure(musician.age)(key = "error.dvalidation.legalage", args = 18)(_ > 18)

    ((notEmpty(musician.name) forAttribute 'name) |@|
      (legalAge forAttribute 'age) |@|
      (atLeastOneString forAttribute 'instruments))(Musician.apply)
  }
}
