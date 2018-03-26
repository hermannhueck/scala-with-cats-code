package sandbox._02_monoids._24_sets

import sandbox._02_monoids.Monoid

object Main24SetMonoids extends App {

  println("---")

  import SetMonoids.setUnionMonoid

  val intSetMonoid = Monoid[Set[Int]]
  val combined = intSetMonoid.combine(Set(1, 2), Set(2, 3))

  println(combined)

  println("---")
}
