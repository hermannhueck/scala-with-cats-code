package sandbox._05_transformers.gabriele

import cats.data.EitherT
import cats.instances.future._
import cats.instances.string._
import cats.syntax.either._
import cats.syntax.eq._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

import scala.language.higherKinds

object Petronella10 extends App {

  println("--- user update with EitherT[Future, MyError, A]")

  final case class MyError(msg: String)

  type ResultT[F[_], A] = EitherT[F, MyError, A]
  type FutureResultT[A] = ResultT[Future, A]

  type FutureResult[A] = Future[Either[MyError, A]]

  final case class User(id: String, name: String, age: Int, nickname: String)
  val gabriele = User("123", "Gabriele", 35, "gabro27")

  def checkUserExists(id: String): FutureResultT[User] = EitherT.fromEither {
    if (id === "123")
      gabriele.asRight
    else
      MyError("sorry, no user").asLeft
  }

  def checkCanBeUpdated(user: User): FutureResultT[User] = EitherT.fromEither {
    if (user.name == "Gabriele")
      user.asRight
    else
      MyError("only Gabriele can be updated").asLeft
  }

  def updateUserOnDb(user: User): FutureResultT[User] = EitherT {
    if (user.name == "Gabriele")
      Future.successful(user.asRight)
    else
      Future.successful(MyError("only Gabriele can be updated").asLeft)
  }

  def updateUser_(id: String): FutureResultT[User] =
    for {
      user <- checkUserExists(id)
      _ <- checkCanBeUpdated(user)
      updatedUser <- updateUserOnDb(user)
    } yield updatedUser

  def updateUser(id: String): FutureResult[User] = updateUser_(id).value

  Await.result(
    updateUser("123"),
    3.seconds
  ).fold(error => println(error.msg), user => println(user))

  Await.result(
    updateUser("333"),
    3.seconds
  ).fold(error => println(error.msg), user => println(user))

  println("-----")
}
