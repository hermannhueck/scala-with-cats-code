package sandbox._04_monads._48_reader

object ReaderMonad extends App {

  println("--- 4.8.1 Creating and Unpacking Readers")

  import cats.data.Reader

  case class Cat(name: String, favoriteFood: String)

  val catName: Reader[Cat, String] = Reader(cat => cat.name)

  println(catName.run(Cat("Garfield", "lasagne")))

  println("--- 4.8.2 Composing Readers")

  val greetKitty: Reader[Cat, String] = catName.map(name => s"Hello ${name}")

  println(greetKitty.run(Cat("Heathcliff", "junk food")))

  val feedKitty: Reader[Cat, String] =
    Reader(cat => s"Have a nice bowl of ${cat.favoriteFood}")

  val greetAndFeed: Reader[Cat, String] =
    for {
      greet <- greetKitty
      feed <- feedKitty
    } yield s"$greet. $feed."

  println(greetAndFeed(Cat("Garfield", "lasagne")))

  println(greetAndFeed(Cat("Heathcliff", "junk food")))

  println("--- 4.8.3 Exercise: Hacking on Readers")

  import cats.syntax.applicative._ // for pure

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  val users = Map(
    1 -> "dade",
    2 -> "kate",
    3 -> "margo"
  )

  val passwords = Map(
    "dade" -> "zerocool",
    "kate" -> "acidburn",
    "margo" -> "secret"
  )

  val db = Db(users, passwords)

  def checkLogin(userId: Int, password: String) =
    for {
      optUsername <- findUsername(userId)
      passwordOk <- optUsername.map { username =>
        checkPassword(username, password)
      }.getOrElse {
        false.pure[DbReader]
      }
    } yield passwordOk

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))
}
