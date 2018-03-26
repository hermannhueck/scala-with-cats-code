package sandbox._07_foldTraverse._71_foldable

object FoldableInScala extends App {

  println("----- 7.1.1 Folds and Folding")

  def show[A](list: List[A]): String =
    list.foldLeft("nil")((accum, item) => s"$item then $accum")

  println(show(Nil))
  println(show(List(1, 2, 3)))

  println(List(1, 2, 3).foldLeft(0)(_ + _))
  println(List(1, 2, 3).foldRight(0)(_ + _))

  println(List(1, 2, 3).foldLeft(0)(_ - _))
  println(List(1, 2, 3).foldRight(0)(_ - _))

  println("----- 7.1.2 Exercise: Reflecting on Folds")

  println(List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)) // == List.resverse
  println(List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)) // == List

  println("----- 7.1.3 Exercise: Scaf-fold-ing Other Methods")

  val l = List(1, 2, 3, 4, 5)

  println("-- filter")

  def filter[A](l: List[A])(p: A => Boolean) = l.foldRight(List.empty[A])((elem, acc) => if (p(elem)) elem :: acc else acc)
  def even(x: Int) = x % 2 == 0
  println(l.filter(even))
  println(filter(l)(even))
  assert(l.filter(even) == filter(l)(even))

  println("-- map")

  def map[A, B](l: List[A])(f: A => B) = l.foldRight(List.empty[B])((elem, acc) => f(elem) :: acc)
  def square(x: Int) = x * x
  println(l.map(square))
  println(map(l)(square))
  assert(l.map(square) == map(l)(square))

  println("-- flatMap")

  def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
    l.foldRight(List.empty[B])((elem, acc) => f(elem) ::: acc)
  def squareList(x: Int) = List(x * x)
  println(l.flatMap(squareList))
  println(flatMap(l)(squareList))
  assert(l.flatMap(squareList) == flatMap(l)(squareList))
  println(flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100)))

  println("-- sumOfInts")

  def sumOfInts(ints: List[Int]) = ints.fold(0)(_ + _)
  println(l.sum)
  println(sumOfInts(l))
  assert(l.sum == sumOfInts(l))

  println("-- sumWithNumeric")

  import scala.math.Numeric

  def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
    list.foldRight(numeric.zero)(numeric.plus)

  println(sumWithNumeric(List(1, 2, 3)))

  println("-- sumWithMonoid")

  import cats.Monoid

  def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
    list.foldRight(monoid.empty)(monoid.combine)

  import cats.instances.int._ // for Monoid

  println(sumWithMonoid(List(1, 2, 3)))

  println("-----")
}
