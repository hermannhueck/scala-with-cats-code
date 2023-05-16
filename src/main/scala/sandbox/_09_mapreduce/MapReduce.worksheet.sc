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

def foldMap[A, B: Monoid](va: Vector[A])(f: A => B): B =
  va.map(f).combineAll // combineAll == foldLeft(empty)(combine)

def foldMap2[A, B: Monoid](as: Vector[A])(f: A => B): B =
  as.map(f).foldLeft(Monoid[B].empty)(_ |+| _)

def foldMap3[A, B: Monoid](as: Vector[A])(f: A => B): B =
  as.foldLeft(Monoid[B].empty)(_ |+| f(_))

foldMap(Vector(1, 2, 3))(identity)
foldMap(Vector(1, 2, 3))(_.toString + "! ")
foldMap("Hello world!".toVector)(_.toString.toUpperCase)

val nCores = Runtime.getRuntime.availableProcessors
s"nCores = $nCores"

(1 to 10).toList.grouped(3).toList
(1 to 32).toList.grouped(32 / nCores).toList

def chunkSize(totalSize: Int, nCores: Int): Int = {
  (1.0 * totalSize / nCores).ceil.toInt
}

def parallelFoldMap[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] = {
  val chunks = values.grouped(chunkSize(values.length, nCores))
  val future = Future.traverse(chunks) { chunk =>
    Future(foldMap(chunk)(f))
  } // Future.traverse using foldMap implemented above
  future map { _.toVector.combineAll }
}

def parallelFoldMap2[A, B: Monoid](values: Vector[A])(f: A => B): Future[B] =
  values
    .grouped(chunkSize(values.length, nCores))
    .toVector
    .traverse { chunk => Future(chunk.foldMap(f)) } // Traverse.traverse using Foldable.foldMap
    .map(_.combineAll)

// ----- foldMap
foldMap((1 to 1000000).toVector)(identity)

// ----- parallelFoldMap
Await.result(
  parallelFoldMap((1 to 1000000).toVector)(identity),
  3.seconds
)

// ----- parallelFoldMap2
Await.result(
  parallelFoldMap2((1 to 1000000).toVector)(identity),
  3.seconds
)
