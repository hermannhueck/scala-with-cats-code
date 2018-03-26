package sandbox._07_foldTraverse._72_traverse

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent._
import scala.concurrent.duration._

object TraverseInCats extends App {

  val hostnames = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration


  {
    println("----- 7.2.3 Traverse in Cats")   // for Traverse

    import cats.Traverse
    import cats.instances.future._ // for Applicative
    import cats.instances.list._   // for Traverse

    val totalUptime: Future[List[Int]] =
      Traverse[List].traverse(hostnames)(getUptime)

    println(Await.result(totalUptime, 1.second))

    val numbers: List[Future[Int]] = List(Future(1), Future(2), Future(3))
    val numbers2: Future[List[Int]] = Traverse[List].sequence(numbers)

    println(Await.result(numbers2, 1.second))

    import cats.syntax.traverse._ // for sequence and traverse

    println(Await.result(hostnames.traverse(getUptime), 1.second))

    println(Await.result(numbers.sequence[Future, Int], 1.second))
  }

  println("-----")
}
