package sandbox._05_transformers.gabriele

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella02 extends App {

  println("--- using Future[A], but should be Future[Option[A]]")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  def getUser(name: String): Future[User] = Future(gabriele) // maybe there is no user with this name
  def getAddress(user: User): Future[Address] = Future(user.address)

  def getCity(name: String): Future[String] = for {
    user <- getUser(name)
    address <- getAddress(user)
  } yield address.city

  println(Await.result(getCity("Gabriele"), 3.seconds))

  println("-----")
}
