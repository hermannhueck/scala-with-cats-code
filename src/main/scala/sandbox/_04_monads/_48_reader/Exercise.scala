package sandbox._04_monads._48_reader

object Exercise extends App {

  import cats.data.Reader
  import cats.syntax.applicative._ // for pure

  println("--- 4.8.3 Exercise: Hacking on Readers")

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

  case class Db(usernames: Map[Int, String], passwords: Map[String, String])
  val db = Db(users, passwords)

  type DbReader[A] = Reader[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    Reader(db => db.usernames.get(userId))

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    Reader(db => db.passwords.get(username).contains(password))

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = // ^= Reader[Db, Boolean] ^= Kleisli[Id, Db, Boolean]
    for {
      optUsername <- findUsername(userId)
      passwordOk <- optUsername
                      .map(un => checkPassword(un, password))
                      .getOrElse(false.pure[DbReader])
    } yield passwordOk

  println(checkLogin(1, "zerocool").run(db))
  println(checkLogin(4, "davinci").run(db))

  println(checkLogin(1, "zerocool")(db))
  println(checkLogin(4, "davinci")(db))

  println("---")
}
