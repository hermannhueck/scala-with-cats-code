package myex.validated.config

import cats.Semigroup
import cats.data.ValidatedNec
import cats.data.Validated
import Validated._
import cats.SemigroupK
import cats.data.NonEmptyChain
import cats.implicits._
import cats.Apply
import cats.Applicative

object Ex01ParallelValidation extends hutil.App {

  def parallelValidate[E: Semigroup, A, B, C](v1: Validated[E, A], v2: Validated[E, B])(
      f: (A, B) => C
  ): Validated[E, C] =
    (v1, v2) match {
      case (Valid(a), Valid(b))       => Valid(f(a, b))
      case (Valid(_), i @ Invalid(_)) => i
      case (i @ Invalid(_), Valid(_)) => i
      case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
    }

  case class ConnectionParams(url: String, port: Int)

  val badConfig = Config(Map(("endpoint", "127.0.0.1"), ("port", "not an int")))

  implicit val necSemigroup: Semigroup[NonEmptyChain[ConfigError]] =
    SemigroupK[NonEmptyChain].algebra[ConfigError]

  implicit val readString: Read[String] = Read.stringRead
  implicit val readInt: Read[Int]       = Read.intRead

  import hutil.syntax.pipe._

  val v1 = parallelValidate(badConfig.parse[String]("url").toValidatedNec, badConfig.parse[Int]("port").toValidatedNec)(
    ConnectionParams.apply
  )
  // v1: cats.data.Validated[cats.data.NonEmptyChain[ConfigError],ConnectionParams] = Invalid(Chain(MissingConfig(url), ParseError(port)))
  v1 | println

  val v2 =
    parallelValidate(badConfig.parse[String]("endpoint").toValidatedNec, badConfig.parse[Int]("port").toValidatedNec)(
      ConnectionParams.apply
    )
  // v2: cats.data.Validated[cats.data.NonEmptyChain[ConfigError],ConnectionParams] = Invalid(Chain(ParseError(port)))
  v2 | println

  val goodConfig = Config(Map(("endpoint", "127.0.0.1"), ("port", "1234")))
  // goodConfig: Config = Config(Map(endpoint -> 127.0.0.1, port -> 1234))

  val v3 =
    parallelValidate(goodConfig.parse[String]("endpoint").toValidatedNec, goodConfig.parse[Int]("port").toValidatedNec)(
      ConnectionParams.apply
    )
  // v3: cats.data.Validated[cats.data.NonEmptyChain[ConfigError],ConnectionParams] = Valid(ConnectionParams(127.0.0.1,1234))
  v3 | println

  println()

  implicit def validatedApplicative[E: Semigroup]: Applicative[Validated[E, *]] =
    new Applicative[Validated[E, *]] {

      def ap[A, B](ff: Validated[E, A => B])(fa: Validated[E, A]): Validated[E, B] =
        (fa, ff) match {
          case (Valid(a), Valid(fab))     => Valid(fab(a))
          case (i @ Invalid(_), Valid(_)) => i
          case (Valid(_), i @ Invalid(_)) => i
          case (Invalid(e1), Invalid(e2)) => Invalid(Semigroup[E].combine(e1, e2))
        }

      def pure[A](x: A): Validated[E, A] = Validated.valid(x)
    }

  // already defined above
  // implicit val necSemigroup: Semigroup[NonEmptyChain[ConfigError]] =
  //   SemigroupK[NonEmptyChain].algebra[ConfigError]

  val config = Config(Map(("name", "cat"), ("age", "not a number"), ("houseNumber", "1234"), ("lane", "feline street")))

  case class Address(houseNumber: Int, street: String)
  case class Person(name: String, age: Int, address: Address)

  val personFromConfig: ValidatedNec[ConfigError, Person] =
    Apply[ValidatedNec[ConfigError, *]].map4(
      config.parse[String]("name").toValidatedNec,
      config.parse[Int]("age").toValidatedNec,
      config.parse[Int]("house_number").toValidatedNec,
      config.parse[String]("street").toValidatedNec
    ) {
      case (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street))
    }
  personFromConfig | println

  val personFromConfigUsingMapN: ValidatedNec[ConfigError, Person] =
    (
      config.parse[String]("name").toValidatedNec,
      config.parse[Int]("age").toValidatedNec,
      config.parse[Int]("house_number").toValidatedNec,
      config.parse[String]("street").toValidatedNec
    ) mapN { (name, age, houseNumber, street) => Person(name, age, Address(houseNumber, street)) }
  personFromConfigUsingMapN | println
}
