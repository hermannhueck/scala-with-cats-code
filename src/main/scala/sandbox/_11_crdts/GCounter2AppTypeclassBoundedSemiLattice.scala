package sandbox._11_crdts

import cats.Monoid
import cats.syntax.foldable._
import cats.syntax.semigroup._
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._

import scala.language.higherKinds

object GCounter2AppTypeclassBoundedSemiLattice extends App {

  trait BoundedSemiLattice[A] extends Monoid[A] {
    def empty: A
    def combine(a1: A, a2: A): A
  }

  object BoundedSemiLattice {

    implicit val counterLattice: BoundedSemiLattice[Int] = new BoundedSemiLattice[Int] {
      override def empty: Int = 0
      override def combine(i1: Int, i2: Int): Int = i1 max i2
    }

    implicit def gsetLattice[A]: BoundedSemiLattice[Set[A]] = new BoundedSemiLattice[Set[A]] {
      override def empty: Set[A] = Set.empty[A]
      override def combine(sa1: Set[A], sa2: Set[A]): Set[A] = sa1 union sa2
    }
  }

  final case class GCounter[A](counters: Map[String, A]) {

    def increment(machine: String, amount: A)(implicit ma: Monoid[A]): GCounter[A] = {
      val value = amount |+| counters.getOrElse(machine, ma.empty)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter[A])(implicit bsl: BoundedSemiLattice[A]): GCounter[A] =
      GCounter(this.counters |+| that.counters)

    def total(implicit ma: Monoid[A]): A =
      counters.values.toList.combineAll
  }

  println("\n-----")

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val merged = GCounter(g1) merge GCounter(g2)
  // merged.counters: Map[String,Int] = Map(a -> 7, b -> 5)
  println(merged.counters)

  val total = merged.total
  // total: Int = 12
  println(total)

  println("-----\n")
}
