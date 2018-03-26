package sandbox._02_monoids._27_summary

import cats.instances.string._
import cats.syntax.semigroup._ // for |+|

object Summary extends App {

  println("---")

  println("Scala" |+| " with " |+| "Cats")

  println("---")

  import cats.instances.int._
  import cats.instances.option._ // for Monoid

  println(Option(1) |+| Option(2))

  println("---")

  import cats.instances.map._ // for Monoid

  val map1 = Map("a" -> 1, "b" -> 2)
  val map2 = Map("b" -> 3, "d" -> 4)

  println(map1 |+| map2)

  println("---")

  import cats.instances.tuple._  // for Monoid

  val tuple1 = ("hello", 123)
  val tuple2 = ("world", 321)

  println(tuple1 |+| tuple2)

  println("---")
}
