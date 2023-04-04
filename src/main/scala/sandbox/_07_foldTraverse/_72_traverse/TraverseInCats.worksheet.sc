import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

val hostnames = List(
  "alpha.example.com",
  "beta.example.com",
  "gamma.demo.com"
)

def getUptime(hostname: String): Future[Int] =
  Future(hostname.length * 60) // just for demonstration

// ----- 7.2.3 Traverse in Cats

import cats.Traverse
import cats.instances.future._ // for Applicative
import cats.instances.list._   // for Traverse

val totalUptime: Future[List[Int]] =
  Traverse[List].traverse(hostnames)(getUptime)

Await.result(totalUptime, 1.second)

val numbers: List[Int]             = List(1, 2, 3)
val listFutures: List[Future[Int]] = numbers.map(Future(_))

val futureList: Future[List[Int]]  = Traverse[List].sequence(listFutures)
val futureList2: Future[List[Int]] = Traverse[List].traverse(numbers)(Future(_))

Await.result(futureList, 1.second)
Await.result(futureList2, 1.second)

import cats.syntax.traverse._ // for sequence and traverse

Await.result(hostnames.traverse(getUptime), 1.second)

Await.result(numbers.traverse[Future, Int](Future(_)), 1.second)
