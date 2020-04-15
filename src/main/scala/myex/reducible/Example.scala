package myex.reducible

import hutil.syntax.pipe._
import cats._
import cats.data._
import cats.implicits._

object Example extends hutil.App {

  Reducible[NonEmptyList].reduce(NonEmptyList.of("a", "b", "c")) | println
  // res0: String = abc

  Reducible[NonEmptyList].reduceMap(NonEmptyList.of(1, 2, 4))(_.toString) | println
  // res1: String = 124

  Reducible[NonEmptyVector].reduceK(NonEmptyVector.of(List(1, 2, 3), List(2, 3, 4))) | println
  // res2: List[Int] = List(1, 2, 3, 2, 3, 4)

  val res3 = Reducible[NonEmptyVector].reduceLeft(NonEmptyVector.of(1, 2, 3, 4))((s, i) => s + i)
  // res3: Int = 10
  println(res3)

  val res4: Int = Reducible[NonEmptyList].reduceRight(NonEmptyList.of(1, 2, 3, 4))((i, s) => Later(s.value + i)).value
  // res4: Int = 10
  println(res4)

  Reducible[NonEmptyList].reduceLeftTo(NonEmptyList.of(1, 2, 3, 4))(_.toString)((s, i) => s + i) | println
  // res5: String = 1234

  Reducible[NonEmptyList]
    .reduceRightTo(NonEmptyList.of(1, 2, 3, 4))(_.toString)((i, s) => Later(s.value + i))
    .value | println
  // res6: String = 4321

  Reducible[NonEmptyList].nonEmptyIntercalate(NonEmptyList.of("a", "b", "c"), ", ") | println
  // res7: String = a, b, c

  def countChars(s: String) =
    s.toCharArray.groupBy(identity).view.mapValues(_.length).toMap
  // countChars: (s: String)scala.collection.immutable.Map[Char,Int]

  Reducible[NonEmptyList].nonEmptyTraverse_(NonEmptyList.of("Hello", "World"))(countChars) | println
  // res8: scala.collection.immutable.Map[Char,Unit] = Map(l -> (), o -> ())

  Reducible[NonEmptyVector].nonEmptyTraverse_(NonEmptyVector.of("Hello", ""))(countChars) | println
  // res9: scala.collection.immutable.Map[Char,Unit] = Map()

  Reducible[NonEmptyList].nonEmptySequence_(NonEmptyList.of(Map(1 -> 'o'), Map(1 -> 'o'))) | println
  // res10: scala.collection.immutable.Map[Int,Unit] = Map(1 -> ())
}
