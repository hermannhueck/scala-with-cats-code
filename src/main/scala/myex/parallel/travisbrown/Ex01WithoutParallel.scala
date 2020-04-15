package myex.parallel.travisbrown

object Ex01WithoutParallel extends hutil.App {

  import scala.util.Try

  case class InvalidSizes(x: Int, y: Int)
      extends Exception(
        s"Error: $x is not smaller than $y!"
      )

  def parseInt(input: String): Either[Throwable, Int] = Try(input.toInt).toEither

  def checkValues(p: (Int, Int)): Either[InvalidSizes, (Int, Int)] =
    if (p._1 >= p._2) Left(InvalidSizes(p._1, p._2)) else Right(p)

  import cats.data.EitherNel
  import cats.instances.either._
  import cats.instances.list._
  import cats.syntax.apply._
  import cats.syntax.either._
  import cats.syntax.traverse._

  def checkParses(p: (String, String)): EitherNel[Throwable, (Int, Int)] =
    (parseInt(p._1).toValidatedNel, parseInt(p._2).toValidatedNel).tupled.toEither

  def parse(input: List[(String, String)]): EitherNel[Throwable, List[(Int, Int)]] =
    input
      .traverse(
        checkParses(_).flatMap(checkValues(_).toEitherNel).toValidated
      )
      .toEither

  import scala.util.chaining._

  val badInput = List(("a", "1"), ("b", "c"), ("1", "0")) tap println

  parse(badInput).leftMap(_.toList.foreach(println))
}
