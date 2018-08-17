package sandbox._11_crdts

import cats.Monoid
import cats.instances.int._
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.semigroup._

import scala.language.higherKinds

object GCounter4AppKeyValueStore extends App {

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

  trait KeyValueStore[F[_, _]] {

    def put[K, V](f: F[K, V])(k: K, v: V): F[K, V]

    def get[K, V](f: F[K, V])(k: K): Option[V]

    def getOrElse[K, V](f: F[K, V])(k: K, default: V): V =
      get(f)(k).getOrElse(default)

    def values[K, V](f: F[K, V]): List[V]
  }

  object KeyValueStore {

    implicit def mapInstance: KeyValueStore[Map] = new KeyValueStore[Map] {

      override def put[K, V](map: Map[K, V])(key: K, value: V): Map[K, V] =
        map + (key -> value)

      override def get[K, V](map: Map[K, V])(key: K): Option[V] =
        map.get(key)

      override def values[K, V](map: Map[K, V]): List[V] =
        map.values.toList
    }

    object syntax {

      implicit class KvsOps[F[_,_], K, V](f: F[K, V]) {

        def put(key: K, value: V)(implicit kvs: KeyValueStore[F]): F[K, V] =
          kvs.put(f)(key, value)

        def get(key: K)(implicit kvs: KeyValueStore[F]): Option[V] =
          kvs.get(f)(key)

        def getOrElse(key: K, default: V)(implicit kvs: KeyValueStore[F]): V =
          kvs.getOrElse(f)(key, default)

        def values(implicit kvs: KeyValueStore[F]): List[V] =
          kvs.values(f)
      }
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

    implicit def gcounterInstance[F[_,_], K, V](implicit KVS: KeyValueStore[F], km: Monoid[F[K, V]]): GCounter[F, K, V] = new GCounter[F, K, V] {

        import KeyValueStore.syntax._

        override def increment(kvs: F[K, V])(k: K, v: V)(implicit m: Monoid[V]): F[K, V] =
          kvs.put(k, v |+| kvs.getOrElse(k, m.empty))

        override def merge(kvs1: F[K, V], kvs2: F[K, V])(implicit bsl: BoundedSemiLattice[V]): F[K, V] =
          kvs1 |+| kvs2 // kv stores not combined correctly

        override def total(kvs: F[K, V])(implicit m: Monoid[V]): V =
          kvs.values.combineAll
      }
  }

  println("\n-----")

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val counter = GCounter[Map, String, Int]

  // !!! wrong values: kv stores not combined correctly
  val merged = counter.merge(g1, g2)
  // merged: Map[String,Int] = Map(a -> 7, b -> 5)
  println(merged)
  println("--> Should be: " + Map("a" -> 7, "b" -> 5))

  val total = counter.total(merged)
  // total: Int = 12
  println(total)
  println("--> Should be: " + 12)

  println("-----\n")
}
