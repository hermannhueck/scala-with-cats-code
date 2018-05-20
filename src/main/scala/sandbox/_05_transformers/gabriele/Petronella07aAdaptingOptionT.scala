package sandbox._05_transformers.gabriele

import cats.data.OptionT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella07aAdaptingOptionT extends App {

  println("--- adapting for OptionT")

  final case class User(name: String, age: Int, nickname: String)
  val gabriele = User("Gabriele", 35, "gabro27")

  def getUser(name: String): Future[Option[User]] = Future(Option(gabriele))

  def getAge(user: User): Future[Int] = Future(user.age)

  def getNickname(user: User): Option[String] = Option(user.nickname)

  def getCity(name: String): OptionT[Future, String] = for {
    user <- OptionT(getUser(name))
    age <- OptionT(getAge(user).map(Option(_)))
    nickname <- OptionT(Future(getNickname(user)))
  } yield s"$nickname: $age"

  println(Await.result(
    getCity("Gabriele").value,
    3.seconds
  ).getOrElse("User or city not found"))

  println("-----")
}
