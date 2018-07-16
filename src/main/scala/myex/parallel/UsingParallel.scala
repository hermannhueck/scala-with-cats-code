/*
  Example copied from Cats documentation:
  https://typelevel.org/cats/typeclasses/parallel.html
 */

package myex.parallel

import cats._, cats.data._, cats.implicits._

object UsingParallel extends App {

  println("\n----- UsingParallel")

  case class Name(value: String)
  case class Age(value: Int)
  case class Person(name: Name, age: Age)

  def parse(s: String): Either[NonEmptyList[String], Int] = {
    if (s.matches("-?[0-9]+")) Right(s.toInt)
    else Left(NonEmptyList.one(s"$s is not a valid integer."))
  }

  def validateAge(a: Int): Either[NonEmptyList[String], Age] = {
    if (a > 18) Right(Age(a))
    else Left(NonEmptyList.one(s"$a is not old enough"))
  }

  def validateName(n: String): Either[NonEmptyList[String], Name] = {
    if (n.length >= 8) Right(Name(n))
    else Left(NonEmptyList.one(s"$n Does not have enough characters"))
  }

  {
    println("\n----- Using mapN and converting between Validated and Either")
    def parsePerson(ageString: String, nameString: String): Either[NonEmptyList[String], Person] =
      for {
        age <- parse(ageString)
        person <- (validateName(nameString).toValidated, validateAge(age).toValidated)
          .mapN(Person)
          .toEither
      } yield person

    val p1 = parsePerson("33", "Wondracheck")
    val p2 = parsePerson("33X", "Wondracheck")
    val p3 = parsePerson("33", "Wumpel")
    val p4 = parsePerson("33X", "Wumpel")
    val p5 = parsePerson("17", "Wondracheck")

    println(p1)
    println(p2)
    println(p3)
    println(p4)
    println(p5)
  }

  {
    println("\n----- Using Parallel's parMapN")
    def parsePerson(ageString: String, nameString: String): Either[NonEmptyList[String], Person] =
      for {
        age <- parse(ageString)
        person <- (validateName(nameString), validateAge(age)).parMapN(Person)
      } yield person

    val p1 = parsePerson("33", "Wondracheck")
    val p2 = parsePerson("33X", "Wondracheck")
    val p3 = parsePerson("33", "Wumpel")
    val p4 = parsePerson("33X", "Wumpel")
    val p5 = parsePerson("17", "Wondracheck")

    println(p1)
    println(p2)
    println(p3)
    println(p4)
    println(p5)
  }

  println("\n----- Traversing with Parallel")
  val traversed = List(Either.left(42), Either.right(NonEmptyList.one("Error 1")), Either.right(NonEmptyList.one("Error 2"))).parSequence
  println(traversed)

  println("\n----- mapN creates a cartesian product")
  val cartesianProduct = (List(1, 2, 3), List(4, 5, 6)).mapN(_ + _)
  println(cartesianProduct)

  println("\n----- parMapN works like a regular zip")
  val zipped = (List(1, 2, 3), List(4, 5, 6)).parMapN(_ + _)
  println(zipped)

  println("\n-----\n")
}
