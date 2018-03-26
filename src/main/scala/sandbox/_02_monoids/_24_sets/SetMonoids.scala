package sandbox._02_monoids._24_sets

import sandbox._02_monoids.{Monoid, Semigroup}

object SetMonoids {

  implicit def setUnionMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty = Set.empty[A]
      def combine(a: Set[A], b: Set[A]): Set[A] = a union b
    }

  implicit val intMonoid: Monoid[Int] = new Monoid[Int] {
    def empty = 0
    def combine(a: Int, b: Int): Int = a + b
  }

  implicit def setIntersectionSemigroup[A]: Semigroup[Set[A]] =
    new Semigroup[Set[A]] {
      def combine(a: Set[A], b: Set[A]): Set[A] = a intersect b
    }

  implicit def symDiffMonoid[A]: Monoid[Set[A]] =
    new Monoid[Set[A]] {
      def empty: Set[A] = Set.empty
      def combine(a: Set[A], b: Set[A]): Set[A] = (a diff b) union (b diff a)
    }
}
