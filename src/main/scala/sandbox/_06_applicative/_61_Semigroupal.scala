package sandbox._06_applicative

object _61_Semigroupal extends App {

  println("----- 6.1 Semigroupal")

  import cats.Semigroupal
  import cats.instances.option._ // for Semigroupal

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

  println("-----")
}
