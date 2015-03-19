package net.atinu.dvalidation

import java.time.{ LocalDate, Period }

import scala.util.{ Success, Failure, Try }
import scalaz.{ Scalaz, Validation }

object BlogSamples extends App {

  trait Classification

  case object StringInstrument extends Classification

  case object Keyboard extends Classification

  abstract class Instrument(val classification: Classification)

  case object BassGuitar extends Instrument(StringInstrument)

  case object Guitar extends Instrument(StringInstrument)

  case object Piano extends Instrument(Keyboard)

  case class Band(name: String)

  case class MemberOfBand(from: LocalDate, membership: Period)

  case class Musician(
    name: String,
    born: LocalDate,
    instruments: Seq[Instrument],
    currentBand: Option[Band] = None,
    formerBands: Seq[MemberOfBand] = Nil)

  def validateAge(birthDate: LocalDate): Option[LocalDate] = {
    if (birthDate.isAfter(LocalDate.now().minusYears(12))) None
    else Some(birthDate)
  }

  /**
   * - Error is communicated as a side effect
   * - Forces client to use try / catch in his code to make it save
   * - Generic error types
   * - Hard to come up with good error messages for a user (translation, localization)
   * - Hard to map the error to an input
   * - hard to reuse validations
   */
  def validateWithIf(musician: Musician): Unit = {
    if (musician.name.trim.isEmpty) throw new IllegalArgumentException("name")
    else if (musician.born.isAfter(LocalDate.now().minusYears(12))) throw new IllegalArgumentException("too young")
    else if (musician.instruments.isEmpty) throw new IllegalArgumentException("at least one instrument")
    else if (musician.currentBand.exists(_.name.isEmpty)) throw new IllegalArgumentException("band must have a name")
  }

  def validateWithTry(musician: Musician): Try[Musician] = {
    if (musician.name.trim.isEmpty) Failure(new IllegalArgumentException("name"))
    else if (musician.born.isAfter(LocalDate.now().minusYears(12))) Failure(new IllegalArgumentException("too young"))
    else if (musician.instruments.isEmpty) Failure(new IllegalArgumentException("at least one instrument"))
    else if (musician.currentBand.exists(_.name.isEmpty)) Failure(new IllegalArgumentException("band must have a name"))
    else Success(musician)
  }

  def validateWithTryCompose(musician: Musician): Try[Musician] = {
    def validBand(band: Band): Try[Band] =
      if (band.name.isEmpty) Failure(new IllegalArgumentException("band must have a name"))
      else Success(band)

    def validCurrentBand(band: Option[Band]) = band match {
      case Some(e) => validBand(e).map(_ => band)
      case None => Success(band)
    }

    def validName(name: String): Try[String] =
      if (name.trim.isEmpty) Failure(new IllegalArgumentException("name"))
      else Success(name)

    def atLeast12(born: LocalDate): Try[LocalDate] =
      if (born.isAfter(LocalDate.now().minusYears(12))) Failure(new IllegalArgumentException("too young"))
      else Success(born)

    def validInstrument(instruments: Seq[Instrument]) =
      if (instruments.isEmpty) Failure(new IllegalArgumentException("at least one instrument"))
      else Success(instruments)

    for {
      band <- validCurrentBand(musician.currentBand)
      name <- validName(musician.name)
      born <- atLeast12(musician.born)
      instruments <- validInstrument(musician.instruments)
    } yield musician
  }

  def validateWithEither(musician: Musician): Either[String, Musician] = {
    def validBand(band: Band): Either[String, Band] =
      if (band.name.isEmpty) Left("band must have a name")
      else Right(band)

    def validIfinBand(band: Option[Band]): Either[String, Option[Band]] = {
      val a: Either[String, Band] = band.toRight("").right.flatMap(band => validBand(band))
      ???
    }

    def validMusician: Either[String, Musician] =
      if (musician.name.trim.isEmpty) Left("name")
      else if (musician.born.isAfter(LocalDate.now().minusYears(12))) Left("too young")
      else if (musician.instruments.isEmpty) Left("at least one instrument")
      else Right(musician)

    for (m <- validMusician.right; b <- validIfinBand(m.currentBand).right) yield m
  }

  def validateWithTryCompose2(musician: Musician): Try[Musician] = {
    def validBand(band: Band): Try[Band] =
      if (band.name.isEmpty) Failure(new IllegalArgumentException("band must have a name"))
      else Success(band)

    def validIfInBand(band: Option[Band]): Try[Option[Band]] =
      if (band.isDefined) validBand(band.get).map(_ => band)
      else Success(band)

    if (musician.name.trim.isEmpty) Failure(new IllegalArgumentException("name"))
    else if (musician.born.isAfter(LocalDate.now().minusYears(12))) Failure(new IllegalArgumentException("too young"))
    else if (musician.instruments.isEmpty) Failure(new IllegalArgumentException("at least one instrument"))
    else if (musician.currentBand.isDefined) validBand(musician.currentBand.get).map(_ => musician)
    else Success(musician)
  }

  type StringValidation[T] = Validation[String, T]

  def validateWithScalazValidation(musician: Musician): StringValidation[Musician] = {
    import scalaz._
    import scalaz.Scalaz._

    def validBand(band: Band): StringValidation[Band] =
      if (band.name.isEmpty) "band must have a name".failure
      else band.success

    def validCurrentBand(band: Option[Band]): StringValidation[Option[Band]] = band match {
      case Some(v) => validBand(v).map(Some(_))
      case None => band.success
    }

    def validName(name: String): StringValidation[String] =
      if (name.trim.isEmpty) "name missing".failure
      else name.success

    def atLeast12(born: LocalDate): StringValidation[LocalDate] =
      if (born.isAfter(LocalDate.now().minusYears(12))) "too young".failure
      else born.success

    def validInstrument(instruments: Seq[Instrument]): StringValidation[Seq[Instrument]] =
      if (instruments.isEmpty) "at least one instrument".failure
      else instruments.success

    (validName(musician.name)
      |@| atLeast12(musician.born)
      |@| validInstrument(musician.instruments)
      |@| validCurrentBand(musician.currentBand)
      |@| musician.formerBands.success)(Musician.apply)
  }

  val opeth = Band("Opeth")
  val mikael = Musician(
    name = "Mikael Åkerfeldt",
    born = LocalDate.parse("1974-04-17"),
    instruments = List(Guitar, BassGuitar),
    currentBand = Option(opeth))

  val badMikael = mikael.copy(born = LocalDate.now.minusYears(2)).copy(instruments = Nil)

  println("try - valid")
  val res1 = validateWithTryCompose(mikael)
  println(res1)

  println("try - invalid")
  val res2 = validateWithTryCompose(badMikael)
  println(res2)

  println("validation - valid")
  println(validateWithScalazValidation(mikael))

  println("validation - invalid")
  println(validateWithScalazValidation(badMikael))
}
