package sandbox._08_asynctesting

import scala.language.higherKinds
import cats.Id
import cats.Applicative
import cats.instances.list._
import cats.syntax.functor._
import cats.syntax.traverse._

import scala.concurrent.Future
import scala.util.{Failure, Success, Try}

trait UptimeClient[F[_]] {
  def getUptime(hostname: String): F[Int]
}

class UptimeService[F[_]: Applicative](client: UptimeClient[F]) {
  def getTotalUptime(hostnames: List[String]): F[Int] =
    hostnames.traverse(client.getUptime).map(_.sum)
}

class RealUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Future] {
  def getUptime(hostname: String): Future[Int] =
    Future.successful(hosts.getOrElse(hostname, 0))
}

class TestUptimeClient(hosts: Map[String, Int]) extends UptimeClient[Id] {
  def getUptime(hostname: String): Id[Int] =
    hosts.getOrElse(hostname, 0)
}

object TestUptime extends App {

  def testTotalUptime(): Unit = {
    val hosts    = Map("host1" -> 10, "host2" -> 6)
    val client   = new TestUptimeClient(hosts)
    val service  = new UptimeService(client)
    val actual   = service.getTotalUptime(hosts.keys.toList)
    val expected = hosts.values.sum
    assert(actual == expected)
  }

  println("\n-----")

  Try {

    testTotalUptime()

  } match {
    case Success(_) =>
      println("Test successful")
    case Failure(t: Throwable) =>
      println(s"Test failed with exception: $t")
  }

  println("-----\n")
}
