package sandbox._04_monads._45_monaderror

import cats.MonadError
import cats.instances.either._ // for MonadError

object MonadErrorHandling extends App {

  println("---")

  type ErrorOr[A] = Either[String, A]

  val monadError = MonadError[ErrorOr, String]

  val success: ErrorOr[Int] = monadError.pure(42)
  println(success)

  val failure: ErrorOr[Nothing] = monadError.raiseError("Badness")
  println(failure)

  val eo1: ErrorOr[ErrorOr[String]] = monadError.handleError(failure) {
    case "Badness" => monadError.pure("It's ok")
    case other => monadError.raiseError("It's not ok")
  }
  println(eo1)

  val eo2: ErrorOr[Int] = monadError.ensure(success)("Number too low!")(_ > 1000)
  println(eo2)

  // using MonadError syntax

  println("---")

  import cats.syntax.applicative._      // for pure
  import cats.syntax.applicativeError._ // for raiseError etc
  import cats.syntax.monadError._       // for ensure

  val success2 = 42.pure[ErrorOr]
  println(success2)

  val failure2 = "Badness".raiseError[ErrorOr, Int]
  println(failure2)

  val eo3 = success2.ensure("Number to low!")(_ > 1000)
  println(eo3)

  val eo4 = failure2.ensure("Number to low!")(_ > 1000)
  println(eo4)

  println("--- 4.5.3 Instances of MonadError")

  {
    import scala.util.Try
    import cats.instances.try_._ // for MonadError

    val exn: Throwable = new RuntimeException("It's all gone wrong")
    val tryy: Try[Int] = exn.raiseError[Try, Int]
    println(tryy)
  }

  println("---")
}
