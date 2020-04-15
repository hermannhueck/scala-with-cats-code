package myex.parallel.travisbrown

object Ex02WithParallel extends hutil.App {

  import scala.util.Try

  case class InvalidSizes(x: Int, y: Int)
      extends Exception(
        s"Error: $x is not smaller than $y!"
      )

  def parseInt(input: String): Either[Throwable, Int] = Try(input.toInt).toEither

  def checkValues(p: (Int, Int)): Either[InvalidSizes, (Int, Int)] =
    if (p._1 >= p._2) Left(InvalidSizes(p._1, p._2)) else Right(p)

  // The Parallel type class really isn’t anything more than a way to generalize this process
  // of going back and forth between monadic and applicative contexts. It allows us to rewrite
  // our checkParses and parse methods without ever referring to Validated:

  import cats.data.EitherNel
  import cats.instances.either._
  import cats.instances.list._
  import cats.instances.parallel._
  import cats.syntax.either._
  import cats.syntax.parallel._

  def checkParses(p: (String, String)): EitherNel[Throwable, (Int, Int)] =
    (parseInt(p._1).toEitherNel, parseInt(p._2).toEitherNel).parTupled

  def parse(input: List[(String, String)]): EitherNel[Throwable, List[(Int, Int)]] =
    input.parTraverse(checkParses(_).flatMap(checkValues(_).toEitherNel))

  import scala.util.chaining._

  val badInput = List(("a", "1"), ("b", "c"), ("1", "0")) tap println

  parse(badInput).leftMap(_.toList.foreach(println))
}
