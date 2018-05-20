package sandbox._05_transformers.gabriele

import cats.data.OptionT
import cats.instances.future._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella07bAdaptingOptionT extends App {

  println("--- using OptionT.liftF and OptionT.fromOption")

  final case class User(name: String, age: Int, nickname: String)
  val gabriele = User("Gabriele", 35, "gabro27")

  def getUser(name: String): Future[Option[User]] = Future(Option(gabriele))

  def getAge(user: User): Future[Int] = Future(user.age)

  def getNickname(user: User): Option[String] = Option(user.nickname)

  def getCity(name: String): OptionT[Future, String] = for {
    user <- OptionT(getUser(name))
    age <- OptionT.liftF(getAge(user)) // same as OptionT(getAge(user).map(Option(_)))
    nickname <- OptionT.fromOption(getNickname(user)) // same as: OptionT(Future(getNickname(user)))
  } yield s"$nickname: $age"

  println(Await.result(
    getCity("Gabriele").value,
    3.seconds
  ).getOrElse("User or city not found"))

  println("-----")
}
