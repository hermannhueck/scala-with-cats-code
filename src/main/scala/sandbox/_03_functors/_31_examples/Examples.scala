package sandbox._03_functors._31_examples

import cats.Functor

import scala.concurrent.{Await, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import cats.instances.list._   // for Functor

object Examples extends App {

  println(Functor[List].map(List(1, 2, 3))(_ + 1))

  println(List(1, 2, 3).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!"))

  println(Option(123).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!"))

  println(Right(123).
    map(n => n + 1).
    map(n => n * 2).
    map(n => n + "!"))

  val future: Future[String] =
    Future(123).
      map(n => n + 1).
      map(n => n * 2).
      map(n => n + "!")

  println(Await.result(future, 1.second))



  import scala.util.Random

  val future1 = {
    // Initialize Random with a fixed seed:
    val r = new Random(0L)

    // nextInt has the side-effect of moving to
    // the next random number in the sequence:
    val x = Future(r.nextInt)

    for {
      a <- x
      b <- x
    } yield (a, b)
  }

  val future2 = {
    val r = new Random(0L)

    for {
      a <- Future(r.nextInt)
      b <- Future(r.nextInt)
    } yield (a, b)
  }

  val result1 = Await.result(future1, 1.second)
  println(result1)

  val result2 = Await.result(future2, 1.second)
  println(result2)



  import cats.instances.function._ // for Functor
  import cats.syntax.functor._     // for map

  val func1: Int => Double =
    (x: Int) => x.toDouble

  val func2: Double => Double =
    (y: Double) => y * 2

  type MyFunc[A] = Int => A
  println( Functor[MyFunc].map(func1)(func2)(1) )

  println((func1 map func2)(1))  // composition

  println((func1 andThen func2)(1)) // composition using andThen
  // res8: Double = 2.0

  println(func2(func1(1)))



  val func =
    ((x: Int) => x.toDouble).
      map(x => x + 1).
      map(x => x * 2).
      map(x => x + "!")

  println(func(123))
}
