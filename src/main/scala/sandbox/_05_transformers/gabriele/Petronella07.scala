package sandbox._05_transformers.gabriele

import cats.data.OptionT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella07 extends App {

  println("--- using OptionT with Future")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  def getUser(name: String): OptionT[Future, User] = OptionT(Future(Option(gabriele)))

  def getAddress(user: User): OptionT[Future, Address] = OptionT(Future(Option(user.address)))

  def getCity(name: String): OptionT[Future, String] = for {
    user <- getUser(name)
    address <- getAddress(user)
  } yield address.city

  println(Await.result(
    getCity("Gabriele").value,
    3.seconds
  ).getOrElse("User or city not found"))

  println("-----")
}
