package sandbox._04_monads._48_reader

object ExerciseWithFunction1 extends App {

  import cats.instances.function._
  import cats.syntax.functor._
  import cats.syntax.flatMap._
  import cats.syntax.applicative._ // for pure

  println("--- 4.8.3 Exercise: Hacking on Function1")

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

  type DbReader[A] = Db => A    // ^= Function1[Db, A]

  def findUsername(userId: Int): DbReader[Option[String]] =
    db => db.usernames.get(userId)

  def checkPassword(username: String, password: String): DbReader[Boolean] =
    db => db.passwords.get(username).contains(password)

  def checkLogin(userId: Int, password: String): DbReader[Boolean] = // ^= Function1[Db, Boolean] ^= DB => Boolean
    for {
      optUsername <- findUsername(userId)
      passwordOk <- optUsername
                      .map(un => checkPassword(un, password))
                      // .getOrElse(false.pure[DbReader])
                      .getOrElse((_:Db) => false)
    } yield passwordOk

  println(checkLogin(1, "zerocool").apply(db))
  println(checkLogin(4, "davinci").apply(db))

  println(checkLogin(1, "zerocool")(db))
  println(checkLogin(4, "davinci")(db))

  println("---")
}
