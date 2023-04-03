// ----- 6.3 Semigroupal Applied to Different Types"

// ----- Future"

import cats.Semigroupal
import cats.instances.future._ // for Semigroupal
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val futurePair = Semigroupal[Future].product(Future("Hello"), Future(123)) // Futures run in parallel

Await.result(futurePair, 1.second)

import cats.syntax.apply._ // for mapN

case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

// --- Futures combined with Semigroupal run in parallel"
val futureCat = ( // Futures run in parallel
  Future("Garfield"),
  Future(1978),
  Future(List("Lasagne"))
).mapN(Cat.apply)

Await.result(futureCat, 1.second)

// ----- List"

import cats.Semigroupal
import cats.instances.list._ // for Semigroupal

// --- Semigroupal.product gives us the Cartesian product of two lists"
val x = Semigroupal[List].product(List(1, 2), List(3, 4))
x

// ----- Either"

import cats.Semigroupal
import cats.instances.either._ // for Semigroupal

type ErrorOr[A] = Either[Vector[String], A]

// --- product sees the first failure and stops"
val x2 = Semigroupal[ErrorOr].product( // product sees the first failure and stops
  Left(Vector("Error 1")),
  Left(Vector("Error 2"))
) // Left(Vector(Error 1))
x2

// ----- 6.3.1 Semigroupal Applied to Monads"

import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

// flatMap then create --> Future run sequentially
val result1 = for {
  x <- Future("Future 1")
  y <- Future("Future 2")
} yield (x, y)
Await.result(result1, 1.second)

// create then flatMap pattern --> Future run in parallel
val a = Future("Future 1")
val b = Future("Future 2")

val result2 = for {
  x <- a
  y <- b
} yield (x, y)
println(Await.result(result2, 1.second))

// ----- 6.3.1.1 Exercise: The Product of Monads, e.g. Lists"

import cats.Monad
import cats.syntax.flatMap._ // for flatMap
import cats.syntax.functor._ // for map
import scala.language.higherKinds

// product for Moands implemented with flatMap and map
def product1[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  x.flatMap(a => y.map(b => (a, b)))

// product for Moands implemented with a for comprehension
def product2[M[_]: Monad, A, B](x: M[A], y: M[B]): M[(A, B)] =
  for {
    a <- x
    b <- y
  } yield (a, b)

import cats.instances.list._
import cats.instances.option._
import cats.instances.future._
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val p1List = product1(List(1, 2, 3), List("a", "b", "c"))
p1List
val p1Opt  = product1(Option(1), Option("a"))
p1Opt
val p1Fut  = product1(Future(1), Future("a"))
Await.ready(p1Fut, 1.second)
p1Fut

val p2List = product2(List(1, 2, 3), List("a", "b", "c"))
p2List
val p2Opt  = product2(Option(1), Option("a"))
p2Opt
val p2Fut  = product2(Future(1), Future("a"))
Await.ready(p2Fut, 1.second)
p2Fut

// -----

Semigroupal[List].product(List(1, 2), List(3, 4))

(List(1, 2), List(3, 4)).tupled

import cats.syntax.parallel._ // for parTupled
(List(1, 2), List(3, 4)).parTupled
