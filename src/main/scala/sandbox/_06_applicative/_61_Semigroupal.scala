package sandbox._06_applicative

import cats.Semigroupal
import cats.instances.option._
import cats.instances.list._

object _61_Semigroupal extends App {

  println("----- 6.1 Semigroupal[Option]")

  {
    val a = Semigroupal[Option].product(Some(123), Some("abc"))
    println(a) // --> Some((123,abc))

    val b = Semigroupal[Option].product(None, Some("abc"))
    println(b) // --> None

    val c = Semigroupal[Option].product(Some(123), None)
    println(c) // --> None

    val d = Semigroupal.tuple3(Option(1), Option(2), Option(3))
    println(d) // --> Some((1,2,3))

    val e = Semigroupal.tuple3(Option(1), Option(2), Option.empty[Int])
    println(e) // --> None

    val f = Semigroupal.map3(Option(1), Option(2), Option(3))(_ + _ + _)
    println(f) // --> Some(6)

    val g = Semigroupal.map2(Option(1), Option.empty[Int])(_ + _)
    println(g) // --> None
  }

  println("----- 6.1 Semigroupal[List]")

  {
    val a = Semigroupal[List].product(List(1, 2, 3), List("a", "b", "c"))
    println(a) // --> List((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))

    val b = Semigroupal[List].product(List(1, 2, 3), List.empty[String])
    println(b) // --> List()

    val c = Semigroupal.tuple2(List(1, 2, 3), List("a", "b", "c"))
    println(c) // --> List((1,a), (1,b), (1,c), (2,a), (2,b), (2,c), (3,a), (3,b), (3,c))

    val d = Semigroupal.tuple3(List(1, 2), List("a", "b"), List(false, true))
    println(d) // --> List((1,a,false), (1,a,true), (1,b,false), (1,b,true), (2,a,false), (2,a,true), (2,b,false), (2,b,true))

    val e = Semigroupal.map2(List(10, 20, 30), List(1, 2, 3))(_ + _)
    println(e) // --> List(11, 12, 13, 21, 22, 23, 31, 32, 33)

    val f = Semigroupal.map3(List(100, 200), List(10, 20), List(1, 2))(_ + _ + _)
    println(f) // --> List(111, 112, 121, 122, 211, 212, 221, 222)
  }
    println("-----")
}
