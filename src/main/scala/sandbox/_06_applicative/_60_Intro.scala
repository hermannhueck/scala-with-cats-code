package sandbox._06_applicative

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object _60_Intro extends App {

  println("----- The Problem")

  import cats.syntax.either._ // for catchOnly

  def parseInt(str: String): Either[String, Int] =
    Either.catchOnly[NumberFormatException](str.toInt).
      leftMap(_ => s"Couldn't read $str")

  println("--- parseInt fails fast on the first error encountered")
  val d = for {
    a <- parseInt("a")
    b <- parseInt("b")
    c <- parseInt("c")
  } yield a + b + c

  println(d)

  println("--- With map and flatMap Futures are processes sequentially")
  val f = for {
    str1 <- Future("Future1")
    str2 <- Future("Future2")
  } yield str1 + " followed by " + str2

  println(Await.result(f, 1.seconds))

  println("-----")
}
