package sandbox._02_monoids._25_monoidsincats

import cats.Semigroup
import cats.Monoid

object MonoidsInCats extends App {

  println("---")

  import cats.instances.string._

  println(Monoid[String].combine("Hi ", "there"))
  println(Monoid.apply[String].combine("Hi ", "there"))

  println(Monoid[String].empty)
  println(Monoid.apply[String].empty)

  println("---")

  println(Semigroup[String].combine("Hi ", "there"))

  println("---")

  import cats.instances.int._
  println(Monoid[Int].combine(32, 10))

  println("---")

  import cats.instances.option._
  val a = Option(22)
  val b = Option(20)
  Monoid[Option[Int]].combine(a, b)

  println("---")

  import cats.syntax.semigroup._ // for |+|
  val stringResult = "Hi " |+| "there" |+| Monoid[String].empty
  println(stringResult)
  val intResult = 1 |+| 2 |+| Monoid[Int].empty
  println(intResult)

  println("---")

  // 2.5.4 Exercise: Adding All The Things

  def add(items: List[Int]): Int =
    items.foldRight(Monoid[Int].empty)(_ |+| _)
  println(add(List(1, 2, 3, 4, 5))) // == 15

  multMonoid()

  private def multMonoid(): Unit = {

    implicit val intMultiplication: Monoid[Int] = new Monoid[Int] {
      override def empty: Int                   = 1
      override def combine(x: Int, y: Int): Int = x * y
    }

    def mult(items: List[Int]): Int = {
      items.foldRight(intMultiplication.empty)(intMultiplication.combine)
    }

    println(mult(List(1, 2, 3, 4, 5)))
  }

  println("---")

  // 2.5.4 Exercise: Adding All The Things, Part 2

  def add2[A](items: List[A])(implicit monoid: Monoid[A]): A = // using implicit
    items.foldLeft(monoid.empty)(_ |+| _)

  def add3[A: Monoid](items: List[A]): A = // same using context bound syntax
    items.foldLeft(Monoid[A].empty)(_ |+| _)

  println(add2(List(1, 2, 3)))
  println(add2(List(Some(1), None, Some(2), None, Some(3))))
  // println(add2(List(Some(1), Some(2), Some(3)))) // complie error

  println(add3(List(1, 2, 3)))
  println(add3(List(Some(1), None, Some(2), None, Some(3))))
  // println(add3(List(Some(1), Some(2), Some(3)))) // complie error

  println("---")

  // 2.5.4 Exercise: Adding All The Things, Part 3

  case class Order(totalCost: Double, quantity: Double)

  implicit val orderMonoid = new Monoid[Order] {
    override def empty: Order                       = new Order(0.0, 0.0)
    override def combine(x: Order, y: Order): Order = Order(x.totalCost + y.totalCost, x.quantity + y.quantity)
  }

  println(add3(List(Order(7, 3), Order(2, 1), Order(8, 4), Order(5, 5))))

  println("---")
}
