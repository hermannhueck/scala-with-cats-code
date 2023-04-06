// ===== Parallel =====
// see: https://typelevel.org/cats/typeclasses/parallel.html

import cats.syntax.all._
import cats.data._

case class Name(value: String)
case class Age(value: Int)
case class Person(name: Name, age: Age)

def parse(s: String): EitherNec[String, Int] = {
  if (s.matches("-?[0-9]+")) Right(s.toInt)
  else Left(NonEmptyChain.one(s"$s is not a valid integer."))
}

def validateAge(a: Int): EitherNec[String, Age] = {
  if (a > 18) Right(Age(a))
  else Left(NonEmptyChain.one(s"$a is not old enough"))
}

def validateName(n: String): EitherNec[String, Name] = {
  if (n.length >= 8) Right(Name(n))
  else Left(NonEmptyChain.one(s"$n Does not have enough characters"))
}

def validatePerson01(name: String, age: Int): EitherNec[String, Person] =
  (validateName(name).toValidated, validateAge(age).toValidated)
    .mapN(Person)
    .toEither

validatePerson01("John Doe", 42)
validatePerson01("John Doe", 10)
validatePerson01("John", 42)
validatePerson01("John", 10)

def validatePerson02(name: String, age: Int): EitherNec[String, Person] =
  (validateName(name), validateAge(age)).parMapN(Person)

validatePerson02("John Doe", 42)
validatePerson02("John Doe", 10)
validatePerson02("John", 42)
validatePerson02("John", 10)

def validatePerson(name: String, age: Int): EitherNec[String, Person] =
  validatePerson01(name, age)

def parsePerson(ageString: String, nameString: String): EitherNec[String, Person] =
  for {
    age    <- parse(ageString)
    person <- validatePerson(nameString, age)
  } yield person

parsePerson("foo", "")
parsePerson("foo", "John Doe")
parsePerson("10", "")
parsePerson("10", "John Doe")
parsePerson("42", "John Doe")

// ----- sequence vs. parSequence

val e1 =
  List(Either.right(42), Either.left(NonEmptyList.one("Error 1")), Either.left(NonEmptyList.one("Error 2"))).sequence
e1
val e2 =
  List(Either.right(42), Either.left(NonEmptyList.one("Error 1")), Either.left(NonEmptyList.one("Error 2"))).parSequence
e2

// ----- mapN vs. parMapN

(List(1, 2, 3), List(4, 5, 6)).mapN(_ + _)
(List(1, 2, 3), List(4, 5, 6)).parMapN(_ + _)

// ----- tupled vs. parTupled

(List(1, 2, 3), List(4, 5, 6)).tupled
(List(1, 2, 3), List(4, 5, 6)).parTupled

// ----- Instances of Parallel and NonEmptyParallel

import cats.{NonEmptyParallel, Parallel}

NonEmptyParallel[List].apply
NonEmptyParallel[Vector].apply
import cats.instances.seq._
NonEmptyParallel[Seq].apply

// Parallel[Stream].applicative // Stream deprecated
Parallel[LazyList].applicative
Parallel[Either[String, *]].applicative

Parallel[Either[String, *]].parallel(Right(42))
Parallel[Either[String, *]].sequential(Validated.Valid(42))
