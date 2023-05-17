package sandbox._10_datavalidation

import cats._
import cats.syntax.either._
import cats.syntax.parallel._ // for parMapN
import cats.syntax.semigroup._

object BasicCombinators_10_5a extends App {

  println(
    "----- 10.5 Solution to: Basic Combinators, Part 5, implementing Or, using Applicative and Either with parMapN"
  )

  sealed trait Check[E, A] extends Product with Serializable {
    import Check._

    def and(that: Check[E, A]): Check[E, A] =
      And(this, that)

    def or(that: Check[E, A]): Check[E, A] =
      Or(this, that)

    // apply is the interpreter.
    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {

      case Pure(f) => f(a)

      case And(left, right) =>
        (left(a), right(a)).parMapN((_, _) => a)

      case Or(left, right) =>
        (left(a), right(a)) match {
          case (Left(e1), Left(e2)) => (e1 |+| e2).asLeft
          case (_, _)               => a.asRight
        }
    }
  }

  object Check {

    final case class Pure[E, A](func: A => Either[E, A])              extends Check[E, A]
    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
    final case class Or[E, A](left: Check[E, A], right: Check[E, A])  extends Check[E, A]

    def pure[E, A](f: A => Either[E, A]): Check[E, A] =
      Pure(f)
  }

  val a: Check[List[String], Int] =
    Check.pure { v =>
      if (v > 2) v.asRight
      else List("Must be > 2").asLeft
    }

  val b: Check[List[String], Int] =
    Check.pure { v =>
      if (v < -2) v.asRight
      else List("Must be < -2").asLeft
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
