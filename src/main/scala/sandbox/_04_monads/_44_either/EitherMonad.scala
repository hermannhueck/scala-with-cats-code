package sandbox._04_monads._44_either

object EitherMonad extends App {

  println("--- 4.4.1 Left and Right Bias")

  {
    val either1: Either[String, Int] = Right(10)
    val either2: Either[String, Int] = Right(32)

    { // Scala 2.11
      println(
        for {
          a <- either1 // .right
          b <- either2 // .right
        } yield a + b
      )
    }

    { // Scala 2.12
      println(
        for {
          a <- either1
          b <- either2
        } yield a + b
      )
    }

    { // Cats with Scala 2.11 or Scala 2.12

      // import cats.syntax.either._ // import not necessary in Scala 2.12 but required in Scala 2.11

      println(
        for {
          a <- either1
          b <- either2
        } yield a + b
      )
    }
  }

  println("--- 4.4.2 Creating Instances")

  {
    import cats.syntax.either._

    val a = 3.asRight[String]
    println(a)

    val b = 4.asRight[String]
    println(b)

    val c = for {
      x <- a
      y <- b
    } yield x * x + y * y
    println(c)

    println("---")

    /*
      def countPositive(nums: List[Int]) =
        nums.foldLeft(Right(0)) { (accumulator, num) =>
          if(num > 0) {
            accumulator.map(_ + 1)
          } else {
            Left("Negative. Stopping!")
          }
        }
      // <console>:21: error: type mismatch;
      //  found   : scala.util.Either[Nothing,Int]
      //  required: scala.util.Right[Nothing,Int]
      //              accumulator.map(_ + 1)
      //                             ^
      // <console>:23: error: type mismatch;
      //  found   : scala.util.Left[String,Nothing]
      //  required: scala.util.Right[Nothing,Int]
      //              Left("Negative. Stopping!")
      //                  ^
     */

    def countPositive(nums: List[Int]) =
      nums.foldLeft(0.asRight[String]) { (accumulator, num) =>
        if (num > 0) {
          accumulator.map(_ + 1)
        } else {
          Left("Negative. Stopping!")
        }
      }

    println(countPositive(List(1, 2, 3)))
    println(countPositive(List(1, -2, 3)))

    println("---")

    val e1: Either[NumberFormatException, Int] = Either.catchOnly[NumberFormatException]("foo".toInt)
    println(e1)

    val e2: Either[Throwable, Nothing] = Either.catchNonFatal(sys.error("Badness"))
    println(e2)

    val e3 = Either.fromTry(scala.util.Try("foo".toInt))
    println(e3)

    val e4 = Either.fromOption[String, Int](None, "Badness")
    println(e4)
  }

  println("--- 4.4.3 Transforming Eithers")

  {
    import cats.syntax.either._

    val e5: Int = "Error".asLeft[Int].getOrElse(0)
    println(e5)

    val e6: Either[String, Int] = "Error".asLeft[Int].orElse(2.asRight[String])
    println(e6)

    val e7: Either[String, Int] = -1.asRight[String].ensure("Must be non-negative!")(_ > 0)
    println(e7)

    val e8: Either[String, Int] = "error".asLeft[Int].recover {
      case str: String => -1
    }
    println(e8)

    val e9: Either[String, Int] = "error".asLeft[Int].recoverWith {
      case str: String => Right(-1)
    }
    println(e9)

    val e10: Either[String, Int] = "foo".asLeft[Int].leftMap(_.reverse)
    println(e10)

    val e11: Either[String, Int] = 6.asRight[String].bimap(_.reverse, _ * 7)
    println(e11)

    val e12: Either[String, Int] = "bar".asLeft[Int].bimap(_.reverse, _ * 7)
    println(e12)

    val e13: Either[String, Int] = 123.asRight[String]
    println(e13)
    println(e13.swap)
  }

  println("--- 4.4.4 Error Handling")

  {
    import cats.syntax.either._

    val e14: Either[String, Int] = for {
      a <- 1.asRight[String]
      b <- 0.asRight[String]
      c <- if (b == 0) "DIV0".asLeft[Int] else (a / b).asRight[String]
    } yield c * 100
    println(e14)

    println("---")

    sealed trait LoginError extends Product with Serializable

    final case class UserNotFound(username: String) extends LoginError

    final case class PasswordIncorrect(username: String) extends LoginError

    case object UnexpectedError extends LoginError

    case class User(username: String, password: String)

    type LoginResult = Either[LoginError, User]

    // Choose error-handling behaviour based on type:
    def handleError(error: LoginError): Unit =
      error match {
        case UserNotFound(u)      => println(s"User not found: $u")
        case PasswordIncorrect(u) => println(s"Password incorrect: $u")
        case UnexpectedError      => println(s"Unexpected error")
      }

    val result1: LoginResult = User("dave", "passw0rd").asRight
    // result1: LoginResult = Right(User(dave,passw0rd))
    println(result1)

    val result2: LoginResult = UserNotFound("dave").asLeft
    // result2: LoginResult = Left(UserNotFound(dave))
    println(result2)

    result1.fold(handleError, println)
    // User(dave,passw0rd)

    result2.fold(handleError, println)
    // User not found: dave
  }

  println("---")
}
