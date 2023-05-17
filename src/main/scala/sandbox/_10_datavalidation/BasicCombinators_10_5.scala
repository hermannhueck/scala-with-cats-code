package sandbox._10_datavalidation

import cats._
import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}
import cats.syntax.validated._
import cats.syntax.semigroup._
import cats.syntax.apply._ // for mapN

object BasicCombinators_10_5 extends App {

  println("----- 10.5 Solution to: Basic Combinators, Part 5, implementing Or, using Applicative and Validated")

  sealed trait Check[E, A] extends Product with Serializable {
    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    // apply is the interpreter.
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {

      case Pure(f) =>
        f(a)

      case And(left, right) =>
        (left(a), right(a)).mapN((_, _) => a)

      case Or(left, right) =>
        (left(a), right(a)) match {
          case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
          case (_, _)                     => a.valid
        }
    }
  }

  object Check {

    final case class Pure[E, A](func: A => Validated[E, A])           extends Check[E, A]
    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
    final case class Or[E, A](left: Check[E, A], right: Check[E, A])  extends Check[E, A]

    def pure[E, A](f: A => Validated[E, A]): Check[E, A] =
      Pure(f)
  }

  val a: Check[List[String], Int] =
    Check.pure { v =>
      if (v > 2) v.valid
      else List("Must be > 2").invalid
    }

  val b: Check[List[String], Int] =
    Check.pure { v =>
      if (v < -2) v.valid
      else List("Must be < -2").invalid
    }

  val checkAnd: Check[List[String], Int] = a and b
  val checkOr: Check[List[String], Int]  = a or b

  println(s"checkAnd(3) == ${checkAnd(3)}")
  println(s"checkAnd(2) == ${checkAnd(2)}")
  println(s"checkAnd(-2) == ${checkAnd(-2)}")
  println(s"checkAnd(-3) == ${checkAnd(-3)}")

  println(s"checkOr(3) == ${checkOr(3)}")
  println(s"checkOr(2) == ${checkOr(2)}")
  println(s"checkOr(-2) == ${checkOr(-2)}")
  println(s"checkOr(-3) == ${checkOr(-3)}")

  println("-----")
}
