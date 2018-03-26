package sandbox._05_transformers

object _52_TransformativeExample extends App {

  import cats.data.OptionT
  import cats.instances.list._     // for Monad
  import cats.syntax.applicative._ // for pure

  type ListOption[A] = OptionT[List, A]

  val result1: ListOption[Int] = OptionT(List(Option(10)))
  println(result1)

  val result2: ListOption[Int] = 32.pure[ListOption]
  println(result2)

  val result3 = result1.flatMap { (x: Int) =>
    result2.map { (y: Int) =>
      x + y
    }
  }
  println(result3)

  val result4 = for {
    x <- result1
    y <- result2
  } yield x + y
  println(result4)

  println("-----")
}
