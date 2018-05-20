package sandbox._05_transformers.gabriele

import cats.Monad
import cats.instances.option._
import cats.syntax.option._
import cats.syntax.applicative._
import cats.instances.future._
import cats.instances.list._

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

import scala.language.higherKinds

object Petronella05cMOpt extends App {

  println("--- using    case class MOpt[M[_]: Monad, A](value: M[Option[A]])")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  case class MOpt[M[_]: Monad, A](value: M[Option[A]])

  object MOpt {

    class MOptMonad[M[_] : Monad] {

      type MOpt2[T] = MOpt[M, T]

      implicit def mOptMonad: Monad[MOpt2] = new Monad[MOpt2] {

        override def pure[A](a: A): MOpt2[A] = MOpt(a.pure[Option].pure[M])

        override def flatMap[A, B](fa: MOpt2[A])(f: A => MOpt2[B]): MOpt2[B] = {

          /* def flatMap1(fa: MOpt2[A])(f: A => MOpt2[B]): MOpt2[B] =
            MOpt(Monad[M].flatMap(fa.value) {
              case None => none[B].pure[M]
              case Some(a) => f(a).value
            }) */

          def flatMap2(fa: MOpt2[A])(f: A => MOpt2[B]): MOpt2[B] =
            MOpt(Monad[M].flatMap(fa.value)(_.fold(none[B].pure[M])(f(_).value)))

          flatMap2(fa)(f)
        }

        override def tailRecM[A, B](a: A)(f: A => MOpt2[Either[A, B]]): MOpt2[B] = ???
      }
    }

    implicit class MOptSyntax[M[_] : Monad, A](moA: MOpt[M, A]) {

      type MOpt2[T] = MOpt[M, T]

      def pure(a: A)(implicit monad: Monad[MOpt2]): MOpt2[A] = monad.pure(a)
      def map[B](f: A => B)(implicit monad: Monad[MOpt2]): MOpt2[B] = monad.map(moA)(f)
      def flatMap[B](f: A => MOpt2[B])(implicit monad: Monad[MOpt2]): MOpt2[B] = monad.flatMap(moA)(f)
    }
  }

  def getCity[M[_]: Monad](name: String): M[Option[String]] = {

    // val mOptMonadInstance: MOpt.MOptMonad[M] = new MOpt.MOptMonad[M]
    // implicit val mOptMonad: Monad[mOptMonadInstance.MOpt2] = mOptMonadInstance.mOptMonad
    implicit val mOptMonad: Monad[MOpt.MOptMonad[M]#MOpt2] = new MOpt.MOptMonad[M].mOptMonad

    def getUser(name: String): MOpt[M, User] = MOpt(gabriele.pure[Option].pure[M]) // or: gabriele.pure[MOpt.MOptMonad[M]#MOpt2]

    def getAddress(user: User): MOpt[M, Address] = MOpt(user.address.pure[Option].pure[M]) // or: user.address.pure[MOpt.MOptMonad[M]#MOpt2]

    def getCity(n: String): M[Option[String]] = (for {
      user <- getUser(n)
      address <- getAddress(user)
    } yield address.city).value

    getCity(name)
  }

  println("--- using List Monad")
  println(getCity[List]("Gabriele").head.getOrElse("User or city not found"))

  println("--- using Future Monad")
  println(
    Await.result(
      getCity[Future]("Gabriele"),
      3.seconds
    ).getOrElse("User or city not found")
  )

  println("-----")
}
