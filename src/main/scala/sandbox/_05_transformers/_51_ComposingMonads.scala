package sandbox._05_transformers

object _51_ComposingMonads {

  import cats.Monad
  import cats.syntax.applicative._ // for pure
  // import cats.syntax.flatMap._     // for flatMap
  import scala.language.higherKinds

  // Hypothetical example. This won't actually compile:
  def compose[M1[_]: Monad, M2[_]: Monad] = {

    type Composed[A] = M1[M2[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] = a.pure[M2].pure[M1]

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
        ??? // Problem! How do we write flatMap?

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }

  // replace M2 by a concrete Monad, Option in this case
  def compose2[M[_]: Monad] = {

    import cats.instances.option._
    // import cats.syntax.option._
    // import cats.syntax.flatMap._
    // import cats.syntax.monad._

    type Composed[A] = M[Option[A]]

    new Monad[Composed] {

      def pure[A](a: A): Composed[A] = a.pure[Option].pure[M]

      def flatMap[A, B](fa: Composed[A])(f: A => Composed[B]): Composed[B] =
        ??? // fa.flatMap(_.fold(None.pure[M])(f))
        // flatMap(fa)((x: Option[A]) => x.fold(none[B].pure[M])(f))

      override def tailRecM[A, B](a: A)(f: A => Composed[Either[A, B]]): Composed[B] = ???
    }
  }
}
