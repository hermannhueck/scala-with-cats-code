package sandbox._04_monads._42_monadsincats

import cats.Monad

object MonadInstances extends App {

  println("---")

  import cats.instances.option._ // for Monad

  println(
    Monad[Option].flatMap(Option(1))(a => Option(a*2))
  )

  import cats.instances.list._ // for Monad

  println(
    Monad[List].flatMap(List(1, 2, 3))(a => List(a, a*10))
  )

  import cats.instances.vector._ // for Monad

  println(
    Monad[Vector].flatMap(Vector(1, 2, 3))(a => Vector(a, a*10))
  )

  import cats.instances.future._ // for Monad
  import scala.concurrent._
  import scala.concurrent.duration._
  import scala.concurrent.ExecutionContext.Implicits.global

  val fm = Monad[Future]

  val future = fm.flatMap(fm.pure(1))(x => fm.pure(x + 2))

  println(Await.result(future, 1.second))

  println("---")
}
