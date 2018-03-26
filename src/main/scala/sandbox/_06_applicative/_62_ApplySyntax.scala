package sandbox._06_applicative

object _62_ApplySyntax extends App {

  {
    println("----- 6.2 Apply Syntax")

    import cats.instances.option._
    import cats.syntax.apply._

    val a = (Option(123), Option("abc")).tupled
    println(a)

    val b = (Option(123), Option("abc"), Option(true)).tupled
    println(b)

    case class Cat(name: String, born: Int, color: String)

    val cat = (
      Option("Garfield"),
      Option(1978),
      Option("Orange & black")
    ).mapN(Cat.apply)
    println(cat)

    val add: (Int, Int) => Int = (a, b) => a + b

    val d = (Option(1), Option(2)).mapN(add)
    println(d)

    // (Option(1), Option(2), Option(3)).mapN(add)
    // <console>:27: error: type mismatch;
    //  found   : (Int, Int) => Int
    //  required: (Int, Int, Int) => ?
    //        (Option(1), Option(2), Option(3)).mapN(add)
    //                                               ^

    // (Option("cats"), Option(true)).mapN(add)
    // <console>:27: error: type mismatch;
    //  found   : (Int, Int) => Int
    //  required: (String, Boolean) => ?
    //        (Option("cats"), Option(true)).mapN(add)
    //                                            ^
  }

  {
    println("----- 6.2.1 Fancy Functors and Apply Syntax")

    import cats.Monoid
    import cats.instances.int._
    import cats.instances.invariant._
    import cats.instances.list._
    import cats.instances.string._
    import cats.syntax.apply._

    case class Cat(name: String, yearOfBirth: Int, favoriteFoods: List[String])

    val tupleToCat: (String, Int, List[String]) => Cat =
      Cat.apply _

    val catToTuple: Cat => (String, Int, List[String]) =
      cat => (cat.name, cat.yearOfBirth, cat.favoriteFoods)

    implicit val catMonoid: Monoid[Cat] = (
      Monoid[String],
      Monoid[Int],
      Monoid[List[String]]
    ).imapN(tupleToCat)(catToTuple)

    import cats.syntax.semigroup._ // for |+|

    val garfield   = Cat("Garfield", 1978, List("Lasagne"))
    val heathcliff = Cat("Heathcliff", 1988, List("Junk Food"))

    println(garfield |+| heathcliff)
  }

  println("-----")
}
