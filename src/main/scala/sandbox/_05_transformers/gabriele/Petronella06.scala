package sandbox._05_transformers.gabriele

import cats.Monad
import cats.instances.list._
import cats.instances.option._
import cats.syntax.applicative._

object Petronella06 extends App {

  println("--- using ListOpt")

  final case class Address(zip: Int, city: String, street: String)
  final case class User(name: String, address: Address)
  val gabriele = User("Gabriele", Address(12345, "Milano", "Scala"))

  final case class ListOpt[A](value: List[Option[A]])

  object ListOpt {

    implicit val listOptMonad: Monad[ListOpt] = new Monad[ListOpt] {

      override def pure[A](a: A): ListOpt[A] = ListOpt(a.pure[Option].pure[List])

      //    override def map[A, B](fa: ListOpt[A])(f: A => B): ListOpt[B] = flatMap(fa)(a => pure(f(a)))
      //    override def map[A, B](fa: FutOpt[A])(f: A => B): FutOpt[B] = FutOpt(fa.value.map(optA => optA.map(f)))

      override def flatMap[A, B](fa: ListOpt[A])(f: A => ListOpt[B]): ListOpt[B] =
        ListOpt(fa.value.flatMap {
          case None => None.pure[List]
          case Some(a) => f(a).value
        })

      override def tailRecM[A, B](a: A)(f: A => ListOpt[Either[A, B]]): ListOpt[B] = ???
    }

    implicit class ListOptSyntax[A](loA: ListOpt[A]) {

      def pure(a: A)(implicit monad: Monad[ListOpt]): ListOpt[A] = monad.pure(a)

      def map[B](f: A => B)(implicit monad: Monad[ListOpt]): ListOpt[B] = monad.map(loA)(f)

      def flatMap[B](f: A => ListOpt[B])(implicit monad: Monad[ListOpt]): ListOpt[B] = monad.flatMap(loA)(f)
    }

  }

  def getUser(name: String): ListOpt[User] = ListOpt(List(Option(gabriele)))

  def getAddress(user: User): ListOpt[Address] = ListOpt(List(Option(user.address)))

  def getCity(name: String): ListOpt[String] = for {
    user <- getUser(name)
    address <- getAddress(user)
  } yield address.city

  println(getCity("Gabriele").value.head.getOrElse("User or city not found"))

  println("-----")
}
