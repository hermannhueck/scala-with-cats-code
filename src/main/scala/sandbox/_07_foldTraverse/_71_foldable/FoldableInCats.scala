package sandbox._07_foldTraverse._71_foldable

import cats.Foldable

object FoldableInCats extends App {

  {
    println("----- 7.1.4 Foldable in Cats")

    import cats.Foldable
    import cats.instances.list._ // for Foldable

    val ints = List(1, 2, 3)

    println(Foldable[List].foldLeft(ints, 0)(_ + _))

    import cats.instances.option._ // for Foldable

    val maybeInt = Option(123)

    println(Foldable[Option].foldLeft(maybeInt, 10)(_ * _))
  }
  
/*    
// not supported in Cats 2.0
  {
    println("----- 7.1.4.1 Folding Right")

    import cats.Eval
    import cats.Foldable

    def bigData = (1 to 100000).toStream

    // bigData.foldRight(0L)(_ + _) // foldRight of Stream is not stack safe
    // java.lang.StackOverflowError ...

    import cats.instances.stream._ // for Foldable

    val eval: Eval[Long] =
      Foldable[LazyList].
        foldRight(bigData, Eval.now(0L)) { (num, eval) =>
          eval.map(_ + num)
        }

    println(eval.value)

    println("----- Stack Safety in the Standard Library")

    println((1 to 100000).toList.foldRight(0L)(_ + _)) // stack safe
    println((1 to 100000).toVector.foldRight(0L)(_ + _)) // stack safe
  }
*/

  {
    println("----- 7.1.4.2 Folding with Monoids")

    import cats.instances.option._
    import cats.instances.list._

    println(Foldable[Option].nonEmpty(Option(42)))
    println(Foldable[List].find(List(1, 2, 3))(_ % 2 == 0))

    import cats.instances.int._ // for Monoid

    println(Foldable[List].combineAll(List(1, 2, 3)))

    import cats.instances.string._ // for Monoid

    println(Foldable[List].foldMap(List(1, 2, 3))(_.toString))

    import cats.instances.vector._ // for Monoid

    val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))

    println((Foldable[List] compose Foldable[Vector]).combineAll(ints))
  }

  {
    println("----- 7.1.4.3 Syntax for Foldable")

    import cats.instances.list._
    import cats.instances.int._ // for Monoid
    import cats.instances.string._ // for Monoid
    import cats.syntax.foldable._ // for combineAll and foldMap

    println(List(1, 2, 3).combineAll)
    println(List(1, 2, 3).foldMap(_.toString))
  }

  println("-----")
}
