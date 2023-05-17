package sandbox._10_datavalidation

import cats._
import cats.syntax.either._
import cats.syntax.semigroup._

object BasicCombinators_10_3a extends App {

  println("----- 10.3a Solution to: Basic Combinators, Part 3, impl with case class wrapping a function")

  final case class CheckF[E, A](f: A => Either[E, A]) {

    def apply(value: A): Either[E, A] = f(value)

    def and(that: CheckF[E, A])(implicit sge: Semigroup[E]): CheckF[E, A] =
      CheckF { a =>
        (this(a), that(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (_, Left(e))         => e.asLeft
          case (Left(e), _)         => e.asLeft
          case (Right(_), Right(_)) => a.asRight
        }
      }
  }

  val a: CheckF[List[String], Int] =
    CheckF { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: CheckF[List[String], Int] =
    CheckF { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
    }

  val check: CheckF[List[String], Int] = a and b

  println(s"check(3) == ${check(3)}")
  println(s"check(2) == ${check(2)}")
  println(s"check(-2) == ${check(-2)}")
  println(s"check(-3) == ${check(-3)}")
  println("-----")
}
