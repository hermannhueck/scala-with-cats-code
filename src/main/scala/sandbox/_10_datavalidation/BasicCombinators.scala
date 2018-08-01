package sandbox._10_datavalidation

import cats._
import cats.data.Validated.{Invalid, Valid}
import cats.data._
import cats.implicits._

object BasicCombinators extends App {

  object solution_12_10_1 {
    println("\n----- 12.10.1 Solution to: Basic Combinators, Part 1")

    trait Check[E, A] {

      def apply(value: A): Either[E, A]

      def and(that: Check[E, A])(implicit me: Semigroup[E]): Check[E, A] = a => (this (a), that(a)) match {
        case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
        case (_, Left(e)) => e.asLeft
        case (Left(e), _) => e.asLeft
        case (Right(a1), Right(_)) => a1.asRight
      }
    }
  }


  object solution_12_10_3a {
    println("----- 12.10.3a Solution to: Basic Combinators, Part 3, impl with case class wrapping a function")

    final case class CheckF[E, A](f: A => Either[E, A]) {

      def apply(value: A): Either[E, A] = f(value)

      def and(that: CheckF[E, A])(implicit me: Semigroup[E]): CheckF[E, A] =
        CheckF { a =>
          (this (a), that(a)) match {
            case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
            case (_, Left(e)) => e.asLeft
            case (Left(e), _) => e.asLeft
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
  }


  object solution_12_10_3b {
    println("----- 12.10.3b Solution to: Basic Combinators, Part 3, impl with ADT")

    sealed trait Check[E, A] extends Product with Serializable {

      def and(that: Check[E, A]): Check[E, A] = And(this, that)

      def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {

          case Pure(f) => f(a)

          case And(left, right) => (left(a), right(a)) match {
            case (Left(e1),  Left(e2))  => (e1 |+| e2).asLeft
            case (Left(e),   Right(a))  => e.asLeft
            case (Right(a),  Left(e))   => e.asLeft
            case (Right(a1), Right(a2)) => a.asRight
          }
        }
    }

    final case class Pure[E, A](func: A => Either[E, A]) extends Check[E, A]

    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

    val a: Check[List[String], Int] =
      Pure { v =>
        if(v > 2) v.asRight
        else List("Must be > 2").asLeft
      }

    val b: Check[List[String], Int] =
      Pure { v =>
        if(v < -2) v.asRight
        else List("Must be < -2").asLeft
      }

    val check: Check[List[String], Int] = a and b

    println(s"check(3) == ${check(3)}")
    println(s"check(2) == ${check(2)}")
    println(s"check(-2) == ${check(-2)}")
    println(s"check(-3) == ${check(-3)}")
  }


  object solution_12_10_4 {
    println("----- 12.10.4 Solution to: Basic Combinators, Part 4, using Applicative and Validated")

    sealed trait Check[E, A] extends Product with Serializable {

      def and(that: Check[E, A]): Check[E, A] = And(this, that)

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {

        case Pure(f) => f(a)

        case And(left, right) => (left(a), right(a)) mapN ((_, _) => a)
      }
    }

    final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]

    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

    val a: Check[List[String], Int] =
      Pure { v =>
        if(v > 2) v.valid
        else List("Must be > 2").invalid
      }

    val b: Check[List[String], Int] =
      Pure { v =>
        if(v < -2) v.valid
        else List("Must be < -2").invalid
      }

    val check: Check[List[String], Int] = a and b

    println(s"check(3) == ${check(3)}")
    println(s"check(2) == ${check(2)}")
    println(s"check(-2) == ${check(-2)}")
    println(s"check(-3) == ${check(-3)}")
  }


  object solution_12_10_5 {
    println("----- 12.10.5 Solution to: Basic Combinators, Part 5, implementing Or")

    sealed trait Check[E, A] extends Product with Serializable {

      def and(that: Check[E, A]): Check[E, A] = And(this, that)

      def or(that: Check[E, A]): Check[E, A] = Or(this, that)

      def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {

        case Pure(f) =>
          f(a)

        case And(left, right) =>
          (left(a), right(a)) mapN ((_, _) => a)

        case Or(left, right) => (left(a), right(a)) match {
          case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
          case (_, _) => Valid(a)
        }
      }
    }
    final case class Pure[E, A](func: A => Validated[E, A]) extends Check[E, A]
    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
    final case class Or[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]

    val a: Check[List[String], Int] =
      Pure { v =>
        if(v > 2) v.valid
        else List("Must be > 2").invalid
      }

    val b: Check[List[String], Int] =
      Pure { v =>
        if(v < -2) v.valid
        else List("Must be < -2").invalid
      }

    val checkAnd: Check[List[String], Int] = a and b
    val checkOr: Check[List[String], Int] = a or b

    println(s"checkAnd(3) == ${checkAnd(3)}")
    println(s"checkAnd(2) == ${checkAnd(2)}")
    println(s"checkAnd(-2) == ${checkAnd(-2)}")
    println(s"checkAnd(-3) == ${checkAnd(-3)}")

    println(s"checkOr(3) == ${checkOr(3)}")
    println(s"checkOr(2) == ${checkOr(2)}")
    println(s"checkOr(-2) == ${checkOr(-2)}")
    println(s"checkOr(-3) == ${checkOr(-3)}")
  }


  solution_12_10_1
  solution_12_10_3a
  solution_12_10_3b
  solution_12_10_4
  solution_12_10_5

  println("-----\n")
}
