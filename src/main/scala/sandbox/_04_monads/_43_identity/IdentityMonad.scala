package sandbox._04_monads._43_identity

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap
import scala.language.higherKinds
import cats.Monad
import cats.Id

object IdentityMonad extends App {

  println("---")

  def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
    for {
      x <- a
      y <- b
    } yield x*x + y*y

  // sumSquare(3, 4)
  println(sumSquare(3 : Id[Int], 4 : Id[Int]))


  println("Dave" : Id[String])
  println(123 : Id[Int])
  println(List(1, 2, 3) : Id[List[Int]])


  val a: Id[Int] = Monad[Id].pure(3)
  println(a)

  val b: Id[Int] = Monad[Id].flatMap(a)(_ + 1)
  println(b)

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  val c: Id[Int] = for {
    x <- a
    y <- b
  } yield x + y
  println(c)

  println("---")
}
