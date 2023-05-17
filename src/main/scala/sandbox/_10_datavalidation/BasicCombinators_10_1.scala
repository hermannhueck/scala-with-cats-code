package sandbox._10_datavalidation

import cats._
import cats.syntax.either._
import cats.syntax.semigroup._

object BasicCombinators_10_1 extends App {

  println("\n----- 10.1 Solution to: Basic Combinators, Part 1")

  trait Check[E, A] {

    def apply(value: A): Either[E, A]

    def and(that: Check[E, A])(implicit sge: Semigroup[E]): Check[E, A] =
      a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2))  => (e1 |+| e2).asLeft
          case (_, Left(e))          => e.asLeft
          case (Left(e), _)          => e.asLeft
          case (Right(a1), Right(_)) => a1.asRight
        }
  }
  println("-----\n")
}
