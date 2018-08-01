package sandbox._10_datavalidation

import cats._
import cats.data._
import cats.implicits._
import Validated.{Invalid, Valid}

object TransformingData extends App {

  println("----- 12.10.5 Transforming Data")

  sealed trait Predicate[E, A] extends Product with Serializable {

    def and(that: Predicate[E, A]): Predicate[E, A] = Predicate.And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Predicate.Or(this, that)

    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = this match {

      case Predicate.Pure(f) =>
        f(a)

      case Predicate.And(left, right) =>
        (left(a), right(a)) mapN ((_, _) => a)

      case Predicate.Or(left, right) => (left(a), right(a)) match {
        case (Invalid(e1), Invalid(e2)) => (e1 |+| e2).invalid
        case (_, _) => Valid(a)
      }
    }
  }
  object Predicate {
    final case class Pure[E, A](func: A => Validated[E, A]) extends Predicate[E, A]
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A]
  }

  val a: Predicate[List[String], Int] =
    Predicate.Pure { v =>
      if(v > 2) v.valid
      else List("Must be > 2").invalid
    }

  val b: Predicate[List[String], Int] =
    Predicate.Pure { v =>
      if(v < -2) v.valid
      else List("Must be < -2").invalid
    }

  val predicateAnd: Predicate[List[String], Int] = a and b
  val predicateOr: Predicate[List[String], Int] = a or b

  println(s"predicateAnd(3) == ${predicateAnd(3)}")
  println(s"predicateAnd(2) == ${predicateAnd(2)}")
  println(s"predicateAnd(-2) == ${predicateAnd(-2)}")
  println(s"predicateAnd(-3) == ${predicateAnd(-3)}")

  println(s"predicateOr(3) == ${predicateOr(3)}")
  println(s"predicateOr(2) == ${predicateOr(2)}")
  println(s"predicateOr(-2) == ${predicateOr(-2)}")
  println(s"predicateOr(-3) == ${predicateOr(-3)}")


  sealed trait Check[E, A, B] {

    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, B]

    def map[C](f: B => C): Check[E, A, C] = Map[E, A, B, C](this, f)

    def flatMap[C](f: B => Check[E, A, C]): FlatMap[E, A, B, C] = FlatMap[E, A, B, C](this, f)

    def andThen[C](that: Check[E, B, C]): AndThen[E, A, B, C] = AndThen[E, A, B, C](this, that)
  }

  final case class Pure[E, A](pred: Predicate[E, A]) extends Check[E, A, A] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, A] =
      pred(in)
  }

  final case class Map[E, A, B, C](check: Check[E, A, B], f: B => C) extends Check[E, A, C] {
    def apply(in: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(in).map(f)
  }

  final case class FlatMap[E, A, B, C](check: Check[E, A, B], f: B => Check[E, A, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check(a).withEither(_.flatMap(b => f(b)(a).toEither))
  }

  final case class AndThen[E, A, B, C](check1: Check[E, A, B], check2: Check[E, B, C]) extends Check[E, A, C] {
    def apply(a: A)(implicit s: Semigroup[E]): Validated[E, C] =
      check1(a).withEither(_.flatMap(b => check2(b).toEither))
  }

  object Check {

    def apply[E, A](pred: Predicate[E, A]): Check[E, A, A] = Pure(pred)
  }

  println("-----\n")
}
