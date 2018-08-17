package sandbox._09_mapreduce

import scala.language.{higherKinds, postfixOps}

import cats.Monoid
import cats.syntax.monoid._
import cats.syntax.foldable._
import cats.syntax.traverse._
import cats.instances.vector._
import cats.instances.future._
import cats.instances.int._
import cats.instances.string._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object MapReduce extends App {

  println("\n-----")

  def foldMap[A, B: Monoid](va: Vector[A])(f: A => B): B =
    va.map(f).combineAll // combineAll == foldLeft(empty)(combine)

  def foldMap2[A, B : Monoid](as: Vector[A])(f: A => B): B =
    as.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

  def foldMap3[A, B : Monoid](as: Vector[A])(f: A => B): B =
    as.foldLeft(Monoid[B].empty)(_ |+| f(_))

  println(  foldMap(Vector(1, 2, 3))(identity)  )
  println(  foldMap(Vector(1, 2, 3))(_.toString + "! ")  )
  println(  foldMap("Hello world!".toVector)(_.toString.toUpperCase)  )

  val processors = Runtime.getRuntime.availableProcessors
  println(  s"processors = $processors"  )

  println(  (1 to 10).toList.grouped(3).toList  )
  println(  (1 to 32).toList.grouped(32/processors).toList  )

  def chunkSize(totalSize: Int): Int = {
    val nCores = Runtime.getRuntime.availableProcessors
    (1.0 * totalSize / nCores).ceil.toInt
  }

  def parallelFoldMap[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] = {
    val chunks = values.grouped(chunkSize(values.length)).toVector
    val future = Future.traverse(chunks) { chunk => Future(foldMap(chunk)(f)) } // Future.traverse using foldMap implemented above
    future map {_.combineAll}
  }

  def parallelFoldMap2[A, B : Monoid](values: Vector[A])(f: A => B): Future[B] =
    values
      .grouped(chunkSize(values.length))
      .toVector
      .traverse { chunk => Future(chunk.foldMap(f)) } // Traverse.traverse using Foldable.foldMap
      .map(_.combineAll)

  println("----- foldMap")
  println(  foldMap((1 to 1000000).toVector)(identity)  )

  println("----- parallelFoldMap")
  val result = Await.result(
    parallelFoldMap((1 to 1000000).toVector)(identity),
    3 seconds
  )
  println(  result  )

  println("----- parallelFoldMap2")
  val result2 = Await.result(
    parallelFoldMap2((1 to 1000000).toVector)(identity),
    3 seconds
  )
  println(  result2  )

  println("-----\n")
}
