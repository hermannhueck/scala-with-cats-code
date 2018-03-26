package sandbox._04_monads._47_writer

object WriterMonad extends App {

  {
    import cats.Id
    import cats.data.{Writer, WriterT}
    import cats.instances.vector._ // for Monoid
    import cats.syntax.applicative._ // for pure
    import cats.syntax.writer._ // tell, writer

    println("--- 4.7.1 Creating and Unpacking Writers")

    // WriterT[Id, Vector[String], Int] == Writer[Vector[String], Int]
    val w: WriterT[Id, Vector[String], Int] = Writer(Vector(
      "It was the best of times",
      "it was the worst of times"
    ), 1859)
    println(w)

    type Logged[A] = Writer[Vector[String], A]

    val l: Logged[Int] = 123.pure[Logged]
    println(l)

    val l2: Writer[Vector[String], Unit] = Vector("msg1", "msg2", "msg3").tell
    println(l2)

    val a: WriterT[Id, Vector[String], Int] = Writer(Vector("msg1", "msg2", "msg3"), 123)

    val b: Writer[Vector[String], Int] = 123.writer(Vector("msg1", "msg2", "msg3"))

    val aResult: Int = a.value
    // aResult: Int = 123
    println(aResult)

    val aLog: Vector[String] = a.written
    // aLog: Vector[String] = Vector(msg1, msg2, msg3)
    println(aLog)

    val (log, result) = b.run
    // log: scala.collection.immutable.Vector[String] = Vector(msg1, msg2, msg3)
    // result: Int = 123

    println("> a: ")
    println(a)
    println(a.value)
    println(a.written)
    println(b.run)

    println("> b: ")
    println(b)
    println(b.value)
    println(b.written)
    println(b.run)

    println("--- 4.7.2 Composing and Transforming Writers")

    val writer1: WriterT[Id, Vector[String], Int] = for {
      a <- 10.pure[Logged]
      _ <- Vector("a", "b", "c").tell
      b <- 32.writer(Vector("x", "y", "z"))
    } yield a + b

    val ran: (Vector[String], Int) = writer1.run
    println(ran)

    val writer2: WriterT[Id, Vector[String], Int] = writer1.mapWritten(_.map(_.toUpperCase))
    val ran2: (Vector[String], Int) = writer2.run
    println(ran2)

    val writer3: WriterT[Id, Vector[String], Int] = writer1.bimap(
      log => log.map(_.toUpperCase),
      res => res * 100
    )
    val ran3: (Vector[String], Int) = writer3.run
    println(ran3)

    val writer4: WriterT[Id, Vector[String], Int] = writer1.mapBoth { (log, res) =>
      val log2 = log.map(_ + "!")
      val res2 = res * 1000
      (log2, res2)
    }
    val ran4: (Vector[String], Int) = writer4.run
    println(ran4)

    val writer5: WriterT[Id, Vector[String], Int] = writer1.reset
    val ran5: (Vector[String], Int) = writer5.run
    println(ran5)

    val writer6: WriterT[Id, Int, Vector[String]] = writer1.swap
    val ran6: (Int, Vector[String]) = writer6.run
    println(ran6)
  }

  println("--- 4.7.3 Exercise: Show Your Working: The Problem")

  {
    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    def factorial(n: Int): Int = {
      val ans = slowly(if (n == 0) 1 else n * factorial(n - 1))
      println(s"fact $n $ans")
      ans
    }

    println("sequential run (not interleaved):")
    factorial(5)

    println("> parallel run (is interleaved):")
    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    Await.result(Future.sequence(Vector(
      Future(factorial(3)),
      Future(factorial(3))
    )), 5.seconds)
  }

  println("--- 4.7.3 Exercise: Show Your Working: The Solution")

  {
    import cats.data.Writer
    import cats.instances.vector._ // for Monoid
    import cats.syntax.applicative._ // for pure

    def slowly[A](body: => A) =
      try body finally Thread.sleep(100)

    type Logged[A] = Writer[Vector[String], A]

    42.pure[Logged]

    import cats.syntax.writer._ // for tell

    Vector("Message").tell

    //import cats.instances.vector._ // for Monoid

    41.pure[Logged].map(_ + 1)

    def factorial(n: Int): Logged[Int] =
      for {
        ans <- if (n == 0) 1.pure[Logged] else slowly(factorial(n - 1).map(_ * n))
        _ <- Vector(s"fact $n $ans").tell
      } yield ans

    println("sequential run:")
    val (log, res) = factorial(5).run
    println("--- log:\n" + log.mkString("\n"))

    println("parallel run (not interleaved):")

    import scala.concurrent._
    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.duration._

    val Vector((logA, ansA), (logB, ansB)) =
      Await.result(Future.sequence(Vector(
        Future(factorial(3).run),
        Future(factorial(5).run)
      )), 5.seconds)
    println("--- logA:\n" + logA.mkString("\n"))
    println("--- logB:\n" + logB.mkString("\n"))
  }

  println("---")
}
