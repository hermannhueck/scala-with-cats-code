package sandbox._02_monoids

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

object Monoid {
  // def apply[A](implicit monoid: Monoid[A]): Monoid[A] = monoid
  def apply[A : Monoid]: Monoid[A] = implicitly
}

abstract class MonoidAdapter[A](override val empty: A)(join: Function2[A, A, A]) extends Monoid[A] {
  override def combine(x: A, y: A): A = join(x, y)
}

object MonoidInstances {

  implicit object SumMonoid extends MonoidAdapter[Int](empty = 0)(join = _ + _)

  implicit object ProductMonoid extends MonoidAdapter[Int](empty = 1)(join = _ * _)

  implicit object StringMonoid extends MonoidAdapter[String](empty = "")(join = _ + _)

  implicit object AllMonoid extends MonoidAdapter[Boolean](empty = true)(join = _ && _)

  implicit object AnyMonoid extends MonoidAdapter[Boolean](empty = false)(join = _ || _)

  abstract class ListMonoid[A] extends MonoidAdapter[List[A]](empty = List.empty[A])(join = _ ++ _)

  implicit object IntListMonoid extends ListMonoid[Int]
}

