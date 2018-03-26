package sandbox._05_transformers.gabriele

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import cats.syntax.either._

object Petronella11 extends App {

  println("--- awful user update with Future[Either[MyError, A]]")

  final case class MyError(msg: String)

  final case class User(id: String, name: String, age: Int, nickname: String)
  val gabriele = User("123", "Gabriele", 35, "gabro27")

  def checkUserExists(id: String): Future[Option[User]] = Future(Option(gabriele))

  def checkCanBeUpdated(user: User): Future[Boolean] = Future(true)

  def updateUserOnDb(user: User): Future[User] = Future(user)

  def updateUser(id: String): Future[Either[MyError, User]] =
    checkUserExists(id).flatMap {
      case None => Future(MyError("User not existing").asLeft)
      case Some(user) => checkCanBeUpdated(user).flatMap { canBeUpdated =>
        if (!canBeUpdated)
          Future(MyError("User cannot be updated").asLeft)
        else
          updateUserOnDb(user).map(_.asRight)
      }
    }

  Await.result(
    updateUser("123"),
    3.seconds
  ).fold(error => println(error.msg), user => println(user))

  println("-----")
}
