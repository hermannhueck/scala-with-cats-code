package sandbox._01_intro._15_eq

object ExampleEq extends App {

  println("\n--> Scala programming error ...\n")

  //val filtered = List(1, 2, 3).map(Option(_)).filter(item => item == 1) // compiler gives a warning, should be: item == Option(1)
  //println(filtered) // --> List(), expected: List(1)

  println("\n--> Comparing Ints ...\n")

  import cats.Eq
  import cats.instances.int._ // for Eq

  val eqInt = Eq[Int]

  println(eqInt.eqv(123, 123))
  println(eqInt.eqv(123, 234))

  // eqInt.eqv(123, "234")    // compile error
  // <console>:18: error: type mismatch;
  //  found   : String("234")
  //  required: Int
  //        eqInt.eqv(123, "234")
  //                       ^

  println("\n--> using interface syntax for === and =!= ...\n")

  import cats.syntax.eq._ // for === and =!=

  println(123 === 123)
  println(123 =!= 123)
  println(123 === 234)
  println(123 =!= 234)

  println("\n--> Comparing Options ...\n")

  import cats.instances.option._ // for Eq

  // Some(1) === None     // compile error
  // <console>:26: error: value === is not a member of Some[Int]
  //        Some(1) === None
  //                ^

  println((Some(1): Option[Int]) === (None: Option[Int]))
  println(Option(1) === Option.empty[Int])

  import cats.syntax.option._ // for some and none

  println(1.some === none[Int])
  println(1.some =!= none[Int])

  println("\n--> Comparing Dates ...\n")

  import java.util.Date
  import cats.instances.long._ // for Eq

  implicit val dateEq: Eq[Date] = Eq.instance[Date](_.getTime === _.getTime)

  val x = new Date() // now
  Thread.sleep(100L)
  val y = new Date() // a bit later than now

  println(x === x)
  println(x =!= x)
  println(x === y)
  println(x =!= y)

  println("\n--> Comparing Cats ...\n")

  import cats.instances.string._ // for Eq

  final case class Cat(name: String, age: Int, color: String)

  implicit val catEq: Eq[Cat] = Eq.instance[Cat] { (cat1, cat2) =>
    cat1.name === cat2.name &&
    cat1.age === cat2.age &&
    cat1.color === cat2.color
  }

  val garfield   = Cat("Garfield", 38, "orange and black")
  val heathcliff = Cat("Heathcliff", 33, "black and white")

  println(garfield === garfield)
  println(garfield =!= garfield)
  println(garfield === heathcliff)
  println(garfield =!= heathcliff)

  println("\n--> Comparing Option[Cat] ...\n")

  println(Option(garfield) === Option(garfield))
  println(Option(garfield) =!= Option(garfield))
  println(Option(garfield) === Option(heathcliff))
  println(Option(garfield) === Option.empty[Cat])

  println()
}
