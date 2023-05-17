package sandbox._10_datavalidation

import cats._
import cats.syntax.either._
import cats.syntax.parallel._ // for parMapN
import cats.syntax.semigroup._

object BasicCombinators_10_4a extends App {

  println("----- 10.4 Solution to: Basic Combinators, Part 4, using Applicative and Either with parMapN")

  sealed trait Check[E, A] extends Product with Serializable {
    import Check._

    def and(that: Check[E, A]): Check[E, A] = And(this, that)

    // apply is the interpreter.
    def apply(a: A)(implicit s: Semigroup[E]): Either[E, A] = this match {

      case Pure(f) => f(a)

      case And(left, right) =>
        (left(a), right(a)).parMapN((_, _) => a)
    }
  }

  object Check {

    final case class And[E, A](left: Check[E, A], right: Check[E, A]) extends Check[E, A]
    final case class Pure[E, A](func: A => Either[E, A])              extends Check[E, A]

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

  val check: Check[List[String], Int] = a and b

  println(s"check(3) == ${check(3)}")
  println(s"check(2) == ${check(2)}")
  println(s"check(-2) == ${check(-2)}")
  println(s"check(-3) == ${check(-3)}")

  // From here we have a number of options:
  //
  // inspect and refactor checks after they are created;
  // move the apply “interpreter” out into its own module;
  // implement alternative interpreters providing other functionality (for example visualizing checks).

  println("-----")
}
