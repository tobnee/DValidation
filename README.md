# DValidation - Scala Domain Validation Toolkit
A little, opinionated domain object validation toolkit on top of [scalaz](https://github.com/scalaz/scalaz)

## Basic Usage
Lets start with this simple domain model: 

```scala
trait Classification
case object StringInstrument extends Classification
case object Keyboard extends Classification

abstract class Instrument(val classification: Classification)
case object BassGuitar extends Instrument(StringInstrument)
case object Guitar extends Instrument(StringInstrument)
case object Piano extends Instrument(Keyboard)

case class Musician(name: String, age: Int, instruments: Seq[Instrument])
```

A way to get started with DValidation is to define an ad-hoc validation using
the predefined combinators:

```scala
val mikael = Musician("Mikael Åkerfeldt", 40, List(Guitar, BassGuitar))
val martin = Musician("Martin Mendez", 17, List(BassGuitar))

val res: DValidation[Musician] = mikael.validateWith(
  notEmpty(mikael.name) forAttribute 'name,
  ensure(mikael.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
  hasElements(mikael.instruments) forAttribute 'instruments
)
```

The object *mikael* is valid and therefore the validation will result in a *Success*:

```scala
Success(User(Mikael Åkerfeldt,40))
```

*martin* on the other hand is too young which is also reflected in the validation result:

```scala
Failure(DomainError(path: /age, value: 17, msgKey: error.dvalidation.legalage, args: 18))
```

## Templated Validation
The definition of reusable validations can be achieved using a *DValidator* which can be 
defined as follows:

```scala
val musicianValidator: DValidator[Musician] = Validator.template[Musician] { musician =>
  musician.validateWith(
  notEmpty(musician.name) forAttribute 'name,
  ensure(musician.age)(key = "error.dvalidation.legalage", args = 18)(_ > 18) forAttribute 'age,
  hasElements(musician.instruments) forAttribute 'instruments
 )
}
musicianValidator(mikael)
musicianValidator(martin)
```

## Sequence Validation
A common issue when validating business objects is the validation of sequences since errors
are either global or have to be mapped to a specific element. In the example the 
*hasElements* combinator is used to define a global validation rule. *validSequence* is
a combinator which applies a given validation to all elements of a sequence. The 
*withValidations* combinator will include the resulting sequence of *DValidations* in the
parent validation context. 

```scala
val max = Musician("Max Mustermann", 29, List(Piano, Guitar))

val stringInstrumentValidator = Validator.template[Instrument](i =>
  ensure(i)(
   key = "error.dvalidation.stringinstrument", 
   args = i.classification)(_.classification == StringInstrument)
)

max.validateWith(
  notEmpty(max.name) forAttribute 'name,
  ensure(max.age)("error.dvalidation.legalage", 18)(_ > 18) forAttribute 'age,
  hasElements(max.instruments) forAttribute 'instruments
).withValidations(
  validSequence(max.instruments, stringInstrumentValidator) forAttribute 'instruments
)
```
```scala
Failure(DomainError(path: /instruments/[0], value: Piano, msgKey: error.dvalidation.stringinstrument, args: Keyboard))
```

## DValidation Basics
A *DValidation* is defined as follows:

```scala
type DValidation[T] = scalaz.Validation[DomainErrors, T]
```

In other words, in the failure case of a *scalaz.Valdation* the *DomainErrors* type is used,
which itself is a list of at least one DomainError. This type has attributes which
allow clients to handle errors appropriately. The *path* attribute helps to map an error to
the domain object which is responsible for the error. The *value* attribute will return the
value of the validated attribute. Together with the *msgKey* and *args* attributes clients 
can build language specific error messages, which can be represented to users.

The library packages some predefined type of DomainErrors like *IsEmptySeqError* which already
chooses appropriate value and msgKey values.
