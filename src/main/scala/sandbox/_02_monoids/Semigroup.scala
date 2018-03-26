package sandbox._02_monoids

trait Semigroup[A] {
  def combine(x: A, y: A): A
}
