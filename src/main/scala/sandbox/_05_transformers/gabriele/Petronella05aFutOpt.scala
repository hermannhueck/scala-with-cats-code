package sandbox._05_transformers.gabriele

import cats.Monad
import cats.instances.future._
import cats.instances.option._
import cats.syntax.applicative._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella05aFutOpt extends App {

  // Functors compose!
  // def compose[F[_]: Functor, G[_]: Functor]: Functor[F[G[_]]] = ???

  // !!! Monads do not compose!!!
  // def compose[F[_]: Monad, G[_]: Monad]: Monad[F[G[_]]] = ???

  println("--- using FutOpt")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  final case class FutOpt[A](value: Future[Option[A]])

  object FutOpt {

    implicit val futOptMonad: Monad[FutOpt] = new Monad[FutOpt] {

      override def pure[A](a: A): FutOpt[A] = FutOpt(a.pure[Option].pure[Future])

      //    override def map[A, B](fa: FutOpt[A])(f: A => B): FutOpt[B] = flatMap(fa)(a => pure(f(a)))
      //    override def map[A, B](fa: FutOpt[A])(f: A => B): FutOpt[B] = FutOpt(fa.value.map(optA => optA.map(f)))

      override def flatMap[A, B](fa: FutOpt[A])(f: A => FutOpt[B]): FutOpt[B] =
        FutOpt(fa.value.flatMap {
          case None => None.pure[Future]
          case Some(a) => f(a).value
        })

      override def tailRecM[A, B](a: A)(f: A => FutOpt[Either[A, B]]): FutOpt[B] = ???
    }

    implicit class FutOptSyntax[A](foA: FutOpt[A]) {
      def pure(a: A)(implicit monad: Monad[FutOpt]): FutOpt[A] = monad.pure(a)
      def map[B](f: A => B)(implicit monad: Monad[FutOpt]): FutOpt[B] = monad.map(foA)(f)
      def flatMap[B](f: A => FutOpt[B])(implicit monad: Monad[FutOpt]): FutOpt[B] = monad.flatMap(foA)(f)
    }
  }

  def getUser(name: String): FutOpt[User] = FutOpt(Future(Option(gabriele)))

  def getAddress(user: User): FutOpt[Address] = FutOpt(Future(Option(user.address)))

  def getCity(name: String): FutOpt[String] = for {
    user <- getUser(name)
    address <- getAddress(user)
  } yield address.city

  println(
    Await.result(
      getCity("Gabriele").value,
      3.seconds
    ).getOrElse("User or city not found")
  )

  println("-----")
}
