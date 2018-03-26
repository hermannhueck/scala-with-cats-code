package sandbox._05_transformers.gabriele

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella03 extends App {

  println("--- using Future[Option[A]] with invocation of option.get")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  def getUser(name: String): Future[Option[User]] = Future(Option(gabriele))

  def getAddress(user: User): Future[Option[Address]] = Future(Option(user.address))

  def getCity(name: String): Future[String] = for {
    maybeUser <- getUser(name)
    maybeAddress <- getAddress(maybeUser.get) // !!! option.get !!!
  } yield maybeAddress.get.city // awful!!! It calls get on the option

  println(Await.result(getCity("Gabriele"), 3.seconds))

  println("-----")
}
