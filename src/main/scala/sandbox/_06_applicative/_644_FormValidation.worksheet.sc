// --- 6.4.4 Exercise: Form Validation

import cats.data.{EitherNec, NonEmptyChain, Validated, ValidatedNec}
import cats.syntax.either._

type FormData = Map[String, String]

type NEC[A] = NonEmptyChain[A]
def NEC[A](a: A): NonEmptyChain[A] = NonEmptyChain(a)

def getValue(name: String)(data: FormData): EitherNec[String, String] =
  data
    .get(name)                                  // : Option[String]
    .toRight(NEC(s"$name field not specified")) // : EitherNec[String, String]

val getName: FormData => EitherNec[String, String] =
  getValue("name")

// --- test getName
getName(Map())
getName(Map("name" -> "Dade Murphy"))

def parseInt(name: String)(data: String): EitherNec[String, Int] =
  Either
    .catchOnly[NumberFormatException](data.toInt)
    .leftMap(_ => NEC(s"$name must be an integer"))

// --- test parseInt
parseInt("age")("11")
parseInt("age")("foo")

def nonBlank(name: String)(data: String): EitherNec[String, String] =
  data
    .asRight[NEC[String]]
    .ensure(NEC(s"$name cannot be blank"))(_.nonEmpty)

// --- test nonBlank
nonBlank("name")("Dade Murphy")
nonBlank("name")("")

def nonNegative(name: String)(data: Int): EitherNec[String, Int] =
  data
    .asRight[NEC[String]]
    .ensure(NEC(s"$name must be non-negative"))(_ >= 0)

// --- test nonNegative
nonNegative("age")(11)
nonNegative("age")(-1)

def readName(data: FormData): EitherNec[String, String] =
  getValue("name")(data)
    .flatMap(nonBlank("name"))

// --- test readName
readName(Map())
readName(Map("name" -> ""))
readName(Map("name" -> "Dade Murphy"))

def readAge(data: FormData): EitherNec[String, Int] =
  getValue("age")(data)
    .flatMap(nonBlank("age"))
    .flatMap(parseInt("age"))
    .flatMap(nonNegative("age"))

// --- test readAge
readAge(Map())
readAge(Map("age" -> "-1"))
readAge(Map("age" -> "11"))

import cats.instances.list._ // for Semigroupal
import cats.syntax.apply._   // for mapN

case class User(name: String, age: Int)

def readUser(data: FormData): EitherNec[String, User] =
  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply _).toEither

// --- test readUser
readUser(Map("age" -> "-1"))
readUser(Map("name" -> "Dave", "age" -> "37"))

import cats.syntax.parallel._ // for Semigroupal

def parReadUser(data: FormData): EitherNec[String, User] =
  (
    readName(data),
    readAge(data)
  ).parMapN(User.apply _)

// --- test parReadUser
parReadUser(Map("age" -> "-1"))
parReadUser(Map("name" -> "Dave", "age" -> "37"))
