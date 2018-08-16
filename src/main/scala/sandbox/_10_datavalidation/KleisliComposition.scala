package sandbox._10_datavalidation

import cats.Monad
import cats.data.Kleisli
import cats.instances.list._

import scala.language.higherKinds

object KleisliComposition extends App {

  object example {

    type A
    type B
    type C
    type F[_]

    val aToB: A => F[B] = ???
    val bToC: B => F[C] = ???

    // Kleisli Composition
    def aToC(implicit M: Monad[F]): A => F[C] = a => M.flatMap(aToB(a))(bToC)
  }

  println("\n----- KleisliComposition")

  val step1: Kleisli[List, Int, Int] =
    Kleisli(x => List(x + 1, x - 1))

  val step2: Kleisli[List, Int, Int] =
    Kleisli(x => List(x, -x))

  val step3: Kleisli[List, Int, Int] =
    Kleisli(x => List(x * 2, x / 2))

  val pipeline = step1 andThen step2 andThen step3

  val result = pipeline.run(20)
  // res2: List[Int] = List(42, 10, -42, -10, 38, 9, -38, -9)

  println(result)

  println("-----")
}
