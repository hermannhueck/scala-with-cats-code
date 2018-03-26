package sandbox._04_monads._42_monadsincats

import cats.instances.option._   // for Monad
import cats.instances.list._     // for Monad
import cats.syntax.applicative._ // for pure
import cats.Monad
import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
import scala.language.higherKinds

object MonadSyntax extends App {

  println("---")


  println(1.pure[Option])

  println(1.pure[List])


  // sumSquare with flatMap and map
  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    a.flatMap(x => b.map(y => x*x + y*y))

  println(sumSquare(Option(3), Option(4)))

  println(sumSquare(List(1, 2, 3), List(4, 5)))


  // sumSquare re-written with a for comprehension
  def sumSquare2[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  println(sumSquare2(Option(3), Option(4)))
  // res10: Option[Int] = Some(25)

  println(sumSquare2(List(1, 2, 3), List(4, 5)))


  println("---")
}
