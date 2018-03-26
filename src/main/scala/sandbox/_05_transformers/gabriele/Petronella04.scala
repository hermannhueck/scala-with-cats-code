package sandbox._05_transformers.gabriele

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella04 extends App {

  println("--- using Future[Option[A]] with nested map")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  def getUser(name: String): Future[Option[User]] = Future(Option(gabriele))

  def getAddress(user: User): Future[Option[Address]] = Future(Option(user.address))

  def getCity(name: String): Future[Option[String]] = for {
    maybeUser <- getUser(name)
    maybeCity <- maybeUser match {
      case None => Future.successful(None)
      case Some(user) => getAddress(user).map(_.map(_.city)) // !!! nested map !!!
    }
  } yield maybeCity

  println(Await.result(
      getCity("Gabriele"),
      3.seconds
    ).getOrElse("User or city not found")
  )

  println("-----")
}
