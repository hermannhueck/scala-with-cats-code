package sandbox._11_crdts

import cats.Monoid
import cats.syntax.semigroup._
import cats.syntax.foldable._
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
/*
*/

//import cats._, cats.data._, cats.implicits._

import scala.language.higherKinds

object GCounter3AppTypeclassGCounter extends App {

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

  trait GCounter[F[_, _], K, V] {

    def increment(fkv: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V]

    def merge(fkv1: F[K, V], fkv2: F[K, V])(implicit bsl: BoundedSemiLattice[V]): F[K, V]

    def total(fkv: F[K, V])(implicit m: Monoid[V]): V
  }

  object GCounter {

    def apply[F[_, _], K, V](implicit counter: GCounter[F, K, V]): GCounter[F, K, V] =
      counter
  }

  implicit def mapGCounter[K, V]: GCounter[Map, K, V] = new GCounter[Map, K, V] {

    override def increment(fkv: Map[K, V])(k: K, v: V)(implicit m: Monoid[V]): Map[K, V] =
      fkv + (k -> (v |+| fkv.getOrElse(k, m.empty)))

    override def merge(map1: Map[K, V], map2: Map[K, V])(implicit bsl: BoundedSemiLattice[V]): Map[K, V] =
      map1 |+| map2

    override def total(map: Map[K, V])(implicit m: Monoid[V]): V =
      map.values.toList.combineAll
  }

  println("\n-----")

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounter[Map, String, Int]

  val merged = counter.merge(g1, g2)
  // merged: Map[String,Int] = Map(a -> 7, b -> 5)
  println(merged)

  val total = counter.total(merged)
  // total: Int = 12
  println(total)

  println("-----\n")
}
