// --- 6.4.4 Exercise: Form Validation

import cats.data.{NonEmptyChain, Validated}
import cats.syntax.either._

type FormData = Map[String, String]

type NEC[A] = NonEmptyChain[A]
def NEC[A](a: A): NonEmptyChain[A] = NonEmptyChain(a)

type EitherNec[A]    = Either[NEC[String], A]
type ValidatedNec[A] = Validated[NEC[String], A]

def getValue(name: String)(data: FormData): EitherNec[String] =
  data
    .get(name)                                  // : Option[String]
    .toRight(NEC(s"$name field not specified")) // : EitherNec[String]

val getName: FormData => EitherNec[String] =
  getValue("name")

// --- test getName
getName(Map())
getName(Map("name" -> "Dade Murphy"))

def parseInt(name: String)(data: String): EitherNec[Int] =
  Either
    .catchOnly[NumberFormatException](data.toInt)
    .leftMap(_ => NEC(s"$name must be an integer"))

// --- test parseInt
parseInt("age")("11")
parseInt("age")("foo")

def nonBlank(name: String)(data: String): EitherNec[String] =
  data
    .asRight[NEC[String]]
    .ensure(NEC(s"$name cannot be blank"))(_.nonEmpty)

// --- test nonBlank
nonBlank("name")("Dade Murphy")
nonBlank("name")("")

def nonNegative(name: String)(data: Int): EitherNec[Int] =
  data
    .asRight[NEC[String]]
    .ensure(NEC(s"$name must be non-negative"))(_ >= 0)

// --- test nonNegative
nonNegative("age")(11)
nonNegative("age")(-1)

def readName(data: FormData): EitherNec[String] =
  getValue("name")(data)
    .flatMap(nonBlank("name"))

// --- test readName
readName(Map())
readName(Map("name" -> ""))
readName(Map("name" -> "Dade Murphy"))

def readAge(data: FormData): EitherNec[Int] =
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

def readUser(data: FormData): ValidatedNec[User] =
  (
    readName(data).toValidated,
    readAge(data).toValidated
  ).mapN(User.apply _)

// --- test readUser
readUser(Map("age" -> "-1"))
readUser(Map("name" -> "Dave", "age" -> "37"))
