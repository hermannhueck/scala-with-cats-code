package sandbox._05_transformers.gabriele

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import cats.syntax.option._

object Petronella08 extends App {

  println("--- awful user update with Future[Option[A]]")

  final case class User(id: String, name: String, age: Int, nickname: String)
  val gabriele = User("123", "Gabriele", 35, "gabro27")

  def checkUserExists(id: String): Future[Option[User]] = Future(Option(gabriele))

  def checkCanBeUpdated(user: User): Future[Boolean] = Future(true)

  def updateUserOnDb(user: User): Future[User] = Future(user)

  def updateUser(id: String): Future[Option[User]] =
    checkUserExists(id).flatMap {
      case None => Future(none)
      case Some(user) => checkCanBeUpdated(user).flatMap { canBeUpdated =>
        if (!canBeUpdated)
          Future(none)
        else
          updateUserOnDb(user).map(_.some)
      }
    }

  println(Await.result(
    updateUser("123"),
    3.seconds
  ).getOrElse("User or city not found"))

  println("-----")
}
