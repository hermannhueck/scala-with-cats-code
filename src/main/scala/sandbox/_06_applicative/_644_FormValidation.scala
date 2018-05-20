package sandbox._06_applicative

object _644_FormValidation extends App {

  println("--- 6.4.4 Exercise: Form Validation")

  import cats.data.Validated
  import cats.syntax.either._

  type FormData = Map[String, String]
  type FailFast[A] = Either[List[String], A]
  type FailSlow[A] = Validated[List[String], A]

  def getValue(name: String)(data: FormData): FailFast[String] =
    data.get(name).toRight(List(s"$name field not specified"))

  val getName: FormData => FailFast[String] = getValue("name")

  println("--- test getName")
  println(getName(Map()))
  println(getName(Map("name" -> "Dade Murphy")))

  def parseInt(name: String)(data: String): FailFast[Int] =
    Either.catchOnly[NumberFormatException](data.toInt).leftMap(_ => List(s"$name must be an integer"))

  println("--- test parseInt")
  println(parseInt("age")("11"))
  println(parseInt("age")("foo"))

  def nonBlank(name: String)(data: String): FailFast[String] =
    data.asRight[List[String]].ensure(List(s"$name cannot be blank"))(_.nonEmpty)

  def nonNegative(name: String)(data: Int): FailFast[Int] =
    data.asRight[List[String]].ensure(List(s"$name must be non-negative"))(_ >= 0)

  println("--- test nonBlank")
  println(nonBlank("name")("Dade Murphy"))
  println(nonBlank("name")(""))
  println("--- test nonNegative")
  println(nonNegative("age")(11))
  println(nonNegative("age")(-1))

  def readName(data: FormData): FailFast[String] =
    getValue("name")(data).
      flatMap(nonBlank("name"))

  def readAge(data: FormData): FailFast[Int] =
    getValue("age")(data).
      flatMap(nonBlank("age")).
      flatMap(parseInt("age")).
      flatMap(nonNegative("age"))

  println("--- test readName")
  println(readName(Map()))
  println(readName(Map("name" -> "")))
  println(readName(Map("name" -> "Dade Murphy")))
  println("--- test readAge")
  println(readAge(Map()))
  println(readAge(Map("age" -> "-1")))
  println(readAge(Map("age" -> "11")))   // for mapN

  import cats.instances.list._ // for Semigroupal
  import cats.syntax.apply._   // for mapN

  case class User(name: String, age: Int)

  def readUser(data: FormData): FailSlow[User] =
    (
      readName(data).toValidated,
      readAge(data).toValidated
    ).mapN(User.apply _)

  println("--- test readUser")
  println(readUser(Map("age" -> "-1")))
  println(readUser(Map("name" -> "Dave", "age" -> "37")))

  println("-----")
}
