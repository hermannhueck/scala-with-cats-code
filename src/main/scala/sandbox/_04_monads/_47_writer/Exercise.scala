package sandbox._04_monads._47_writer

object Exercise extends App {

  println("--- 4.7.3 Exercise: Show Your Working: The Problem")

  {
    def slowly[A](body: => A) =
      try body
      finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly {
        if (n == 0) 1
        else n * factorial(n - 1)
      }
      println(s"fact $n $ans")
      ans
    }

    println("sequential run (not interleaved):")
    factorial(5)

    println("> parallel run (is interleaved):")
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    Await.result(
      Future.sequence(
        Vector(
          Future(factorial(3)),
          Future(factorial(3))
        )
      ),
      5.seconds
    )
  }

  println("--- 4.7.3 Exercise: Show Your Working: The Solution")

  {
    import cats.data.Writer
    import cats.instances.vector._
    import cats.syntax.applicative._ // for pure

    def slowly[A](body: => A) =
      try body
      finally Thread.sleep(100)

    type Logged[A] = Writer[Vector[String], A]

    42.pure[Logged]

    import cats.syntax.writer._ // for tell

    Vector("Message").tell

    //import cats.instances.vector._ // for Monoid

    41.pure[Logged].map(_ + 1)

    def factorial1(n: Int): Logged[Int] =
      for {
        ans <- if (n == 0)
                1.pure[Logged]
              else
                slowly(factorial(n - 1).map(_ * n))
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    def factorial2(n: Int): Logged[Int] =
      for {
        ans <- if (n == 0)
                1.pure[Logged]
              else
                slowly(factorial(n - 1).mapBoth { (log, ans) => (log :+ s"fact $n $ans", ans * n) })
      } yield ans

    def factorial(n: Int): Logged[Int] =
      factorial2(n)

    println("sequential run:")
    val (log, res) = factorial(5).run
    println("--- log:\n" + log.mkString("\n"))

    println("parallel run (not interleaved):")

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent._
    import scala.concurrent.duration._

    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(
        Future.sequence(
          Vector(
            Future(factorial(3).run),
            Future(factorial(5).run)
          )
        ),
        5.seconds
      )
    println("--- logA:\n" + logA.mkString("\n"))
    println("--- logB:\n" + logB.mkString("\n"))
  }

  println("---")
}
