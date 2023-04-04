import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration

// ----- 7.2.1 Traversing with Futures

val allUptimes1: Future[List[Int]] =
  hostnames.foldLeft(Future(List.empty[Int])) { (flAccum, host) =>
    val fUptime: Future[Int] = getUptime(host) // create Future before flatMap
    for {
      lAccum: List[Int] <- flAccum
      uptime: Int       <- fUptime
    } yield lAccum :+ uptime
  }

Await.result(allUptimes1, 1.second)

val allUptimes2: Future[List[Int]] =
  Future.traverse(hostnames)(getUptime)

Await.result(allUptimes2, 1.second)

val allUptimes3: Future[List[Int]] = {
  val seqFutUptimes: Seq[Future[Int]] = hostnames.map(getUptime)
  val futSeqUptimes: Future[Seq[Int]] = Future.sequence(seqFutUptimes)
  futSeqUptimes.map(_.toList)
}

Await.result(allUptimes3, 1.second)

// ----- 7.2.2 Traversing with Applicatives

import cats.Applicative
import cats.instances.future._   // for Applicative
import cats.syntax.applicative._ // for pure

val flEmpty: Future[List[Int]] =
  List.empty[Int].pure[Future]

def oldCombine(flAccum: Future[List[Int]], host: String): Future[List[Int]] = {
  val fUptime: Future[Int] = getUptime(host) // create Future before flatMap
  for {
    lAccum: List[Int] <- flAccum
    uptime: Int       <- fUptime
  } yield lAccum :+ uptime
}

val totalUptime1: Future[List[Int]] = hostnames.foldLeft(flEmpty)(oldCombine)
Await.result(totalUptime1, 1.second)

import cats.syntax.apply._ // for mapN

// Combining accumulator and hostname using an Applicative:
def newCombine(accum: Future[List[Int]], host: String): Future[List[Int]] =
  (accum, getUptime(host)).mapN(_ :+ _)

val totalUptime2: Future[List[Int]] = hostnames.foldLeft(flEmpty)(newCombine)
Await.result(totalUptime2, 1.second)

import scala.language.higherKinds

def listTraverse[F[_]: Applicative, A, B](list: List[A])(func: A => F[B]): F[List[B]] =
  list.foldLeft(List.empty[B].pure[F]) { (accum, item) =>
    (accum, func(item)).mapN(_ :+ _)
  }

val totalUptime3 = listTraverse(hostnames)(getUptime)
Await.result(totalUptime3, 1.second)

def listSequence[F[_]: Applicative, B](list: List[F[B]]): F[List[B]] =
  listTraverse(list)(identity)

val totalUptime4 = listSequence(hostnames.map(getUptime))
Await.result(totalUptime4, 1.second)

// ----- 7.2.2.1 Exercise: Traversing with Vectors

import cats.instances.vector._ // for Applicative

listSequence(List(Vector(1, 2), Vector(3, 4)))
// --> Vector(List(1, 3), List(1, 4), List(2, 3), List(2, 4))

listSequence(List(Vector(1, 2), Vector(3, 4), Vector(5, 6)))
// --> Vector(List(1, 3, 5), List(1, 3, 6), List(1, 4, 5), List(1, 4, 6), List(2, 3, 5), List(2, 3, 6), List(2, 4, 5), List(2, 4, 6))

// ----- 7.2.2.2 Exercise: Traversing with Options

import cats.instances.option._ // for Applicative

def processOptions(inputs: List[Int]): Option[List[Int]] =
  listTraverse(inputs) { n =>
    if (n % 2 == 0)
      Some(n)
    else
      None
  }

processOptions(List(2, 4, 6)) // --> Some(List(2, 4, 6))
processOptions(List(1, 2, 3)) // --> None

// ----- 7.2.2.3a Exercise: Traversing with Validated

import cats.data.Validated
import cats.instances.list._ // for Monoid

type ErrorsOr[A] = Validated[List[String], A]

def processValidated(inputs: List[Int]): ErrorsOr[List[Int]] = // collects and returns all errors
  listTraverse(inputs) { n =>
    if (n % 2 == 0)
      Validated.valid(n)
    else
      Validated.invalid(List(s"$n is not even"))
  }

processValidated(List(2, 4, 6)) // --> Valid(List(2, 4, 6))
processValidated(List(1, 2, 3)) // --> Invalid(List(1 is not even, 3 is not even))

// ----- 7.2.2.3b Exercise: Traversing with Either

import cats.instances.either._
import cats.syntax.either._

type ErrorOr[A] = Either[List[String], A]

def processEither(inputs: List[Int]): ErrorOr[List[Int]] = // returns only the 1st error
  listTraverse(inputs) { n =>
    if (n % 2 == 0)
      n.asRight[List[String]]
    else
      List(s"$n is not even").asLeft[Int]
  }

processEither(List(2, 4, 6)) // --> Right(List(2, 4, 6))
processEither(List(1, 2, 3)) // --> Left(List(1 is not even))
