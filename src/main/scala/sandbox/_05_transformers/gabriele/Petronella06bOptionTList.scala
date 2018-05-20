package sandbox._05_transformers.gabriele

import cats.data.OptionT
import cats.instances.list._

object Petronella06bOptionTList extends App {

  println("--- using OptionT with List")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  def getUser(name: String): OptionT[List, User] = OptionT(List(Option(gabriele)))

  def getAddress(user: User): OptionT[List, Address] = OptionT(List(Option(user.address)))

  def getCity(name: String): OptionT[List, String] = for {
    user <- getUser(name)
    address <- getAddress(user)
  } yield address.city

  println(getCity("Gabriele").value.head.getOrElse("User or city not found"))

  println("-----")
}
