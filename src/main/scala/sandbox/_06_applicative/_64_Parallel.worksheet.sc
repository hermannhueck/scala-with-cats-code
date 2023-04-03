import cats.Semigroupal
import cats.instances.either._ // for Semigroupal

type ErrorOr[A] = Either[Vector[String], A]

val error1: ErrorOr[Int] = Left(Vector("Error 1"))
val error2: ErrorOr[Int] = Left(Vector("Error 2"))

Semigroupal[ErrorOr].product(error1, error2)

import cats.syntax.apply._     // for tupled
import cats.instances.vector._ // for Semigroup on Vector

(error1, error2).tupled

import cats.syntax.parallel._ // for parTupled

(error1, error2).parTupled

import cats.instances.list._ // for Semigroup on List

type ErrorOrList[A] = Either[List[String], A]

val errStr1: ErrorOrList[Int] = Left(List("error 1"))
val errStr2: ErrorOrList[Int] = Left(List("error 2"))

(errStr1, errStr2).parTupled

val success1: ErrorOr[Int] = Right(1)
val success2: ErrorOr[Int] = Right(2)

val addTwo = (x: Int, y: Int) => x + y

(error1, error2).parMapN(addTwo)

(success1, success2).parMapN(addTwo)
