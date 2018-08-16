package sandbox._10_datavalidation

import cats.Semigroup
import cats.data.Validated._
import cats.data.{Kleisli, NonEmptyList, Validated}
import cats.instances.either._
import cats.syntax.apply._
import cats.syntax.semigroup._
import cats.syntax.validated._   // for Semigroupal

object MyKleislis extends App {

  println("----- 12.10.11/12 Kleislis")

  sealed trait Predicate[E, A] {

    import Predicate._

    def apply(input: A)(implicit s: Semigroup[E]): Validated[E, A]

    // Kleisli need a run and a Monad => we convert the Validated to Either
    def run(implicit s: Semigroup[E]): A => Either[E, A] = (a: A) => this(a).toEither

    def and(that: Predicate[E, A]): Predicate[E, A] = And(this, that)

    def or(that: Predicate[E, A]): Predicate[E, A] = Or(this, that)
  }

  object Predicate {

    final case class Pure[E, A](f: A => Validated[E, A]) extends Predicate[E, A] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        f(a)
    }
    final case class And[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] =
        (left(a), right(a)) mapN ((_, _) => a)
    }
    final case class Or[E, A](left: Predicate[E, A], right: Predicate[E, A]) extends Predicate[E, A] {
      override def apply(a: A)(implicit s: Semigroup[E]): Validated[E, A] = (left(a), right(a)) match {
        case (Valid(a1), _) => Valid(a1)
        case (_, Valid(a2)) => Valid(a2)
        case (Invalid(e1), Invalid(e2)) => Invalid(e1 |+| e2)
      }
    }

    def apply[E, A](f: A => Validated[E, A]): Predicate[E, A] = Pure(f)

    def apply[E, A](f: A => Boolean, err: E): Predicate[E, A] =
      Pure(a => if (f(a)) a.valid else err.invalid)
  }

  object Combinators {
    type Errors = NonEmptyList[String]

    def error(s: String): NonEmptyList[String] =
      NonEmptyList(s, Nil)

    type Result[A] = Either[Errors, A]

    type Check[A, B] = Kleisli[Result, A, B]

    def check[A, B](f: A => Result[B]): Check[A, B] = Kleisli(f)

    def checkPred[A](pred: Predicate[Errors, A]): Check[A, A] = Kleisli[Result, A, A](pred.run)

    def longerThan(n: Int): Predicate[Errors, String] =
      Predicate(_.length > n, error(s"Must be longer than $n characters"))

    val alphanumeric: Predicate[Errors, String] =
      Predicate(_.forall(_.isLetterOrDigit), error(s"Must be all alphanumeric characters"))

    def contains(char: Char): Predicate[Errors, String] =
      Predicate(_.contains(char), error(s"Must contain the character $char"))

    def containsOnce(char: Char): Predicate[Errors, String] =
      Predicate(_.count(_ == char) == 1, error(s"Must contain the character $char only once"))
  }

  import Combinators._

  // A username must contain at least four characters
  // and consist entirely of alphanumeric characters
  //
  val checkUsername: Check[String, String] =
    checkPred(longerThan(3) and alphanumeric)

  // An email address must contain a single `@` sign.
  // Split the string at the `@`.
  // The string to the left must not be empty.
  // The string to the right must be
  // at least three characters long and contain a dot.
  //
  val splitEmail: Check[String, (String, String)] =
    check(_.split('@') match {
      case Array(name, domain) => Right((name, domain))
      case _ => Left(error("Must contain a single @ character"))
    })

  val checkLeft: Check[String, String] =
    checkPred(longerThan(0))

  val checkRight: Check[String, String] =
    checkPred(longerThan(3) and contains('.'))

  val joinEmail: Check[(String, String), String] =
    check { case (l, r) =>
        (checkLeft(l), checkRight(r)).mapN(_ + "@" + _)
    }

  val checkEmail: Check[String, String] =
    splitEmail andThen joinEmail

  final case class User(username: String, email: String)

  def createUser(username: String, email: String): Either[Errors, User] =
    (checkUsername(username), checkEmail(email)).mapN(User)

  val user1: Either[Errors, User] = createUser("Noel", "noel@underscore.io")
  // u1: cats.data.Validated[wrapper.Errors,User] = Valid(User(Noel,noel@underscore.io))
  println(user1)

  val user2: Either[Errors, User] = createUser("", "dave@underscore@io")
  // u2: cats.data.Validated[wrapper.Errors,User] = Invalid(NonEmptyList(Must be longer than 3 characters, Must contain a single @ character))
  println(user2)

  println("-----\n")
}
