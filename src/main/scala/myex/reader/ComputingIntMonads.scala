package myex.reader

import scala.language.higherKinds

import cats._
import cats.implicits._

object ComputingIntMonads extends App {

  def intsTupled[F[_]: Monad](m1: F[Int], m2: F[Int], m3: F[Int]): F[(Int, Int, Int)] =
    for {
      x <- m1
      y <- m2
      z <- m3
    } yield (x, y, z)

  def computeInts[F[_]: Monad](m1: F[Int], m2: F[Int], m3: F[Int]): F[Int] =
    for {
      x <- m1
      y <- m2
      z <- m3
    } yield x + y + z

  println("\n----- F[Int] --> List[Int]")
  val l1 = computeInts(List(1, 2), List(3, 4), List(5, 6))
  // l1: List[Int] = List(9, 10, 10, 11, 10, 11, 11, 12)
  println(l1)

  val l2 = computeInts(List(1, 2), Nil: List[Int], List(5, 6))
  // l2: List[Int] = List()
  println(l2)

  val l3 = computeInts(List(1, 2), List.empty[Int], List(5, 6))
  // l3: List[Int] = List()
  println(l3)

  println("----- F[Int] --> Option[Int]")
  val o1 = computeInts(Option(1), Option(2), Option(3))
  // o1: Option[Int] = Some(6)
  println(o1)

  val o2 = computeInts(Option(1), None: Option[Int], Option(3))
  // o2: Option[Int] = None
  println(o2)

  val o3 = computeInts(Option(1), Option.empty[Int], Option(3))
  // o3: Option[Int] = None
  println(o3)

  val o4 = computeInts(1.some, none[Int], 3.some)
  // o4: Option[Int] = None
  println(o4)

  println("----- F[Int] --> Either[String, Int]")
  val e1 = computeInts(Right(1): Either[String, Int], Right(2): Either[String, Int], Right(3): Either[String, Int])
  // e1: Either[String,Int] = Right(6)
  println(e1)

  val e2 = computeInts(1.asRight[String], 2.asRight[String], 3.asRight[String])
  // e2: Either[String,Int] = Right(6)
  println(e2)

  val e3 = computeInts(Right(1): Either[String, Int], Left("Error"): Either[String, Int], Right(3): Either[String, Int])
  // e3: Either[String,Int] = Left(Error)
  println(e3)

  val e4 = computeInts(1.asRight[String], "Error".asLeft[Int], 3.asRight[String])
  // e4: Either[String,Int] = Left(Error)
  println(e4)

  println("----- F[Int] --> Tuple[String, Int]")
  val t1 = computeInts(("one", 1), ("two", 2), ("three", 3))
  // t1: (String, Int) = (onetwothree,6)
  println(t1)

  val t2 = computeInts((List("one"), 1), (List("two"), 2), (List("three"), 3))
  // t2: (List[String], Int) = (List(one, two, three),6)
  println(t2)

  val t3 = computeInts((1, 1), (2, 2), (3, 3))
  // t3: (Int, Int) = (6,6)
  println(t3)

  // val t4 = computeInts((true, 1), (false, 2), (true, 3))
  // println(t4)

  println("----- F[Int] --> Function1[String, Int]")
  val f1 = computeInts((s:String) => s.length, (s:String) => s.toInt, (s:String) => s.toInt * 10)
  // f1: String => Int = cats.instances.Function1Instances$$anon$2$$Lambda$4679/1244368004@1d533ef3
  println(f1)
  println(f1("2")) // --> 23

  println("----- Composing functions")

  def composeFunctions: Int => String =
    for {
      i1 <- (_:Int) - 5
      i2 <- (_:Int) * 3
      i3 <- (_:Int) + 2
    } yield s"Result: $i1 + $i2 + $i3 = " + (i1 + i2 + i3)

  val result = composeFunctions(3)
  println(result)

  println("-----\n")
}
