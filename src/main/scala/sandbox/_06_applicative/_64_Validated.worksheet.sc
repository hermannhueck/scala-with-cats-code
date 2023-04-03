// ----- 6.4 Validated

import cats.Semigroupal
import cats.data.Validated
import cats.instances.list._ // for Monoid

type AllErrorsOr[A] = Validated[List[String], A]

Semigroupal[AllErrorsOr].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)
Semigroupal[Validated[List[String], *]].product(
  Validated.invalid(List("Error 1")),
  Validated.invalid(List("Error 2"))
)

// ----- 6.4.1 Creating Instances of Validated

import cats.data.Validated

// --- using apply method (return type is Valid or Invalid

Validated.Valid(123)

Validated.Invalid(List("Badness"))

// --- using smart constructors (widening the return type to Validated)

Validated.valid(123) // or: Validated.valid[List[String], Int](123)

Validated.invalid(List("Badness")) // or:  // or: Validated.valid[List[String], Int](123)

// --- using cat.syntax.validated

import cats.syntax.validated._ // for valid and invalid

123.valid[List[String]]

List("Badness").invalid[Int]

// --- using pure and raiseError

import cats.syntax.applicative._      // for pure
import cats.instances.list._          // for raiseError
import cats.syntax.applicativeError._ // for raiseError

type ErrorsOr[A] = Validated[List[String], A]

123.pure[ErrorsOr]

List("Badness").raiseError[ErrorsOr, Int]

// --- creating from Exception, Try, Either, Option

Validated.catchOnly[NumberFormatException]("foo".toInt)

Validated.catchNonFatal(sys.error("Badness"))

Validated.fromTry(scala.util.Try("foo".toInt))

Validated.fromEither[String, Int](Left("Badness"))

Validated.fromOption[String, Int](None, "Badness")

// --- 6.4.2 Combining Instances of Validated

import cats.data.Validated
import cats.Semigroupal

type AllErrorsOr2[A] = Validated[String, A]

// If Semigroup for the left type of Validated is not in scope, you get the following compile error message:
// Semigroupal[AllErrorsOr2]
// <console>:28: error: could not find implicit value for parameter instance: cats.Semigroupal[AllErrorsOr2]
//        Semigroupal[AllErrorsOr2]

import cats.instances.string._ // for Semigroup
import cats.syntax.apply._     // for tupled
import cats.syntax.validated._ // for valid and invalid

Semigroupal[AllErrorsOr2]

(
  "Error 1".invalid[Int],
  "Error 2".invalid[Int]
).tupled

import cats.instances.vector._ // for Semigroupal

val v7 = (
  Vector(404).invalid[Int],
  Vector(500).invalid[Int]
).tupled
println(v7)

import cats.data.{NonEmptyChain, NonEmptyList, NonEmptyVector}

(
  NonEmptyVector.of("Error 1").invalid[Int],
  NonEmptyVector.of("Error 2").invalid[Int]
).tupled
(
  NonEmptyList.of("Error 1").invalid[Int],
  NonEmptyList.of("Error 2").invalid[Int]
).tupled
(
  NonEmptyChain.of("Error 1").invalid[Int],
  NonEmptyChain.of("Error 2").invalid[Int]
).tupled

// --- 6.4.3 Methods of Validated

import cats.syntax.validated._ // for valid and invalid

// --- map
123.valid.map(_ * 100)

// --- leftMap
"?".invalid.leftMap(_ + "!")

// --- biMap
123.valid[String].bimap(_ + "!", _ * 100)
"?".invalid[Int].bimap(_ + "!", _ * 100)

// --- flatMap is not available, as Validated is not a Monad
// --- but Validated has a method 'andThen' with the same signature as flatMap
32.valid.andThen { a =>
  10.valid.map { b =>
    a + b
  }
}

// --- toEither and toValidated

import cats.syntax.either._ // for toValidated

"Badness".invalid[Int]
"Badness".invalid[Int].toEither
"Badness".invalid[Int].toEither.toValidated

// --- withEither (for temporary conversion to Either)

println(41.valid[String].withEither(_.flatMap(n => Right(n + 1))))

// --- ensure

println(123.valid[String].ensure("Negative!")(_ > 0))
println(-123.valid[String].ensure("Negative!")(_ > 0))

// --- getOrElse and fold

println("fail".invalid[Int].getOrElse(0))
println("fail".invalid[Int].fold(_ + "!!!", _.toString))
