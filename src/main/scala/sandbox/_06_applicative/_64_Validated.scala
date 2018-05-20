package sandbox._06_applicative

object _64_Validated extends App {

  {
    println("----- 6.4 Validated")

    import cats.Semigroupal
    import cats.data.Validated
    import cats.instances.list._ // for Monoid

    type AllErrorsOr[A] = Validated[List[String], A]

    val a = Semigroupal[AllErrorsOr].product(
      Validated.invalid(List("Error 1")),
      Validated.invalid(List("Error 2"))
    )
    println(a)
  }

  {
    println("----- 6.4.1 Creating Instances of Validated")

    import cats.data.Validated


    println("--- using apply method (return type is Valid or Invalid")

    val v1 = Validated.Valid(123)
    println(v1)

    val i1 = Validated.Invalid(List("Badness"))
    println(i1)


    println("--- using smart constructors (widening the return type to Validated)")

    val v2 = Validated.valid(123) // or: Validated.valid[List[String], Int](123)
    println(v2)

    val i2 = Validated.invalid(List("Badness")) // or:  // or: Validated.valid[List[String], Int](123)
    println(i2)


    println("--- using cat.syntax.validated")

    import cats.syntax.validated._ // for valid and invalid

    val v3 = 123.valid[List[String]]
    println(v3)

    val i3 = List("Badness").invalid[Int]
    println(i3)


    println("--- using pure and raiseError")

    import cats.syntax.applicative._      // for pure
    import cats.instances.list._ // for raiseError
    import cats.syntax.applicativeError._ // for raiseError

    type ErrorsOr[A] = Validated[List[String], A]

    val v4 = 123.pure[ErrorsOr]
    println(v4)

    val i4 = List("Badness").raiseError[ErrorsOr, Int]
    println(i4)


    println("--- creating from Exception, Try, Either, Option")

    val i5: Validated[NumberFormatException, Int] = Validated.catchOnly[NumberFormatException]("foo".toInt)
    println(i5)

    val i6: Validated[Throwable, Nothing] = Validated.catchNonFatal(sys.error("Badness"))
    println(i6)

    val i7: Validated[Throwable, Int] = Validated.fromTry(scala.util.Try("foo".toInt))
    println(i7)

    val i8: Validated[String, Int] = Validated.fromEither[String, Int](Left("Badness"))
    println(i8)

    val i9: Validated[String, Int] = Validated.fromOption[String, Int](None, "Badness")
    println(i9)
  }

  {
    println("--- 6.4.2 Combining Instances of Validated")

    import cats.data.Validated
    import cats.Semigroupal

    type AllErrorsOr[A] = Validated[String, A]

    // Semigroupal[AllErrorsOr]
    // <console>:28: error: could not find implicit value for parameter instance: cats.Semigroupal[AllErrorsOr]
    //        Semigroupal[AllErrorsOr]
    //

    import cats.instances.string._ // for Semigroup
    import cats.syntax.apply._ // for tupled
    import cats.syntax.validated._ // for valid and invalid

    val v1 = Semigroupal[AllErrorsOr]
    println(v1)

    val v2 = (
      "Error 1".invalid[Int],
      "Error 2".invalid[Int]
    ).tupled
    println(v2)

    import cats.instances.vector._ // for Semigroupal

    val v3 = (
      Vector(404).invalid[Int],
      Vector(500).invalid[Int]
    ).tupled
    println(v3)

    import cats.data.NonEmptyVector

    val v4 = (
      NonEmptyVector.of("Error 1").invalid[Int],
      NonEmptyVector.of("Error 2").invalid[Int]
    ).tupled
    println(v4)
  }

  {
    println("--- 6.4.3 Methods of Validated")

    import cats.syntax.validated._ // for valid and invalid

    println("--- map")
    println(123.valid.map(_ * 100))

    println("--- leftMap")
    println("?".invalid.leftMap(_.toString))

    println("--- biMap")
    println(123.valid[String].bimap(_ + "!", _ * 100))
    println("?".invalid[Int].bimap(_ + "!", _ * 100))

    println("--- flatMap is not available, as Validated is not a Monad")
    println()

    println("--- toEither and toValidated")

    import cats.syntax.either._ // for toValidated

    println("Badness".invalid[Int])
    println("Badness".invalid[Int].toEither)
    println("Badness".invalid[Int].toEither.toValidated)

    println("--- withEither (for temporary conversion to Either)")

    println(41.valid[String].withEither(_.flatMap(n => Right(n + 1))))

    println("--- ensure")

    println(123.valid[String].ensure("Negative!")(_ > 0))
    println(-123.valid[String].ensure("Negative!")(_ > 0))

    println("--- getOrElse and fold")

    println("fail".invalid[Int].getOrElse(0))
    println("fail".invalid[Int].fold(_ + "!!!", _.toString))
  }

  println("-----")
}
