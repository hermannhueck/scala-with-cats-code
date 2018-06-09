package sandbox._05_transformers

import scala.language.higherKinds

import cats.Monad
import cats.instances.option._

object _51_ComposingMonads {

  // Hypothetical example
  def composeFWithF2Impossible[F[_]: Monad, F2[_]: Monad] = {

    type Composed[A] = F[F2[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] = Monad[F].pure(Monad[F2].pure(a))

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] = ???
      // !!! Problem! How do we write flatMap? Impossible to implement!!!

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

  // We can compose the Monads if we know the higher kinded type of the inner Monad.
  // If we replace F2 by a concrete Monad like Option, flatMap can be implemented
  def composeFWithOption[F[_]: Monad] = {

    type Composed[A] = F[Option[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] =  Monad[F].pure(Monad[Option].pure(a))

      def flatMap[A, B](fOptA: Composed[A])(f: A => Composed[B]): Composed[B] =
        Monad[F].flatMap(fOptA) {
          case None => Monad[F].pure(Option.empty[B])
          case Some(a) => f(a)
        } // same as:
        // Monad[F].flatMap(fOptA)(_.fold(Monad[F].pure(Option.empty[B]))(f))

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
}
