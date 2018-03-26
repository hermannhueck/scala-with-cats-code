package sandbox._02_monoids._23_boolean

import sandbox._02_monoids.Monoid

object BooleanMonoids {

  implicit val booleanAndMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean): Boolean = a && b
      def empty: Boolean = true
    }

  implicit val booleanOrMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean): Boolean = a || b
      def empty: Boolean = false
    }

  implicit val booleanEitherMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean): Boolean = (a && !b) || (!a && b)
      def empty: Boolean = false
    }

  implicit val booleanXnorMonoid: Monoid[Boolean] =
    new Monoid[Boolean] {
      def combine(a: Boolean, b: Boolean): Boolean = (!a || b) && (a || !b)
      def empty: Boolean = true
    }
}
