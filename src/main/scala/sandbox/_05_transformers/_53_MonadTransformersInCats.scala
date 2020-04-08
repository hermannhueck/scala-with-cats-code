package sandbox._05_transformers

import cats.data.EitherT

object _53_MonadTransformersInCats extends App {

  {
    println("----- ErrorOrOption")

    import cats.data.OptionT
    import cats.instances.either._
    import cats.syntax.applicative._

    type ErrorOr[A]       = Either[String, A]
    type ErrorOrOption[A] = OptionT[ErrorOr, A]

    // // Create using apply:
    val a = OptionT[ErrorOr, Int](Right(Some(10)))
    // val a = 10.pure[ErrorOrOption]

    val b = 32.pure[ErrorOrOption]

    val c = a.flatMap(x => b.map(y => x + y))
    println(c)       // OptionT(Right(Some(42)))
    println(c.value) // Right(Some(42))

    // Mapping over the Either in the stack:
    val d = c.value.map(_.getOrElse(-1))
    println(d) // Right(42)
  }

  {
    println("----- FutureEitherOption")

    import cats.data.{EitherT, OptionT}
    import cats.instances.future._
    import cats.syntax.applicative._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    type FutureEither[A]       = EitherT[Future, String, A]
    type FutureEitherOption[A] = OptionT[FutureEither, A]

    val futureEitherOr: FutureEitherOption[Int] =
      for {
        a <- 10.pure[FutureEitherOption]
        b <- 32.pure[FutureEitherOption]
      } yield a + b

    Await.ready(
      futureEitherOr.value.value,
      3.seconds
    )

    println(futureEitherOr) // OptionT(EitherT(Future(Success(Right(Some(42))))))

    println(futureEitherOr.value) // EitherT(Future(Success(Right(Some(42)))))

    println(futureEitherOr.value.value) // Future(Success(Right(Some(42))))

    val future: Future[Either[String, Option[Int]]] = futureEitherOr.value.value

    val result: Either[String, Option[Int]] = Await.result(
      futureEitherOr.value.value,
      3.seconds
    ) // Right(Some(42))

    println(result)
    result foreach println
    result foreach { opt => opt foreach println }
  }

  {
    println("----- Kind Projector")

    import cats.instances.option._
    import cats.syntax.applicative._

    val x = 123.pure[EitherT[Option, String, *]]
    println(x)
    println(x.value)
  }

  {
    println("----- FutureEitherOption without type aliases using Kind Projector")

    import cats.data.{EitherT, OptionT}
    import cats.instances.future._

    import scala.concurrent.ExecutionContext.Implicits.global
    import scala.concurrent.{Await, Future}
    import scala.concurrent.duration._

    def wrap[A](value: A): OptionT[EitherT[Future, String, *], A] = {
      val option: Option[A]                             = Some(value)
      val either: Either[String, Option[A]]             = Right(option)
      val future: Future[Either[String, Option[A]]]     = Future(either)
      val etf: EitherT[Future, String, Option[A]]       = EitherT[Future, String, Option[A]](future)
      val otetf: OptionT[EitherT[Future, String, *], A] = OptionT[EitherT[Future, String, *], A](etf)
      otetf
    }

    val futureEitherOr: OptionT[EitherT[Future, String, *], Int] =
      for {
        a <- wrap(10)
        b <- wrap(32)
      } yield a + b

    Await.ready(
      futureEitherOr.value.value,
      3.seconds
    )

    println(futureEitherOr) // OptionT(EitherT(Future(Success(Right(Some(42))))))

    println(futureEitherOr.value) // EitherT(Future(Success(Right(Some(42)))))

    println(futureEitherOr.value.value) // Future(Success(Right(Some(42))))

    val future: Future[Either[String, Option[Int]]] = futureEitherOr.value.value

    val result: Either[String, Option[Int]] = Await.result(
      futureEitherOr.value.value,
      3.seconds
    ) // Right(Some(42))

    println(result)
    result foreach println
    result foreach { opt => opt foreach println }
  }

  {
    println("----- Usage Patterns")

    import cats.data.Writer
    import cats.instances.list._

    type Logged[A] = Writer[List[String], A]

    // Methods generally return untransformed stacks:
    def parseNumber(str: String): Logged[Option[Int]] =
      util.Try(str.toInt).toOption match {
        case Some(num) => Writer(List(s"Read $str"), Some(num))
        case None      => Writer(List(s"Failed on $str"), None)
      }

    // Consumers use monad transformers locally to simplify composition:
    def addAll(a: String, b: String, c: String): Logged[Option[Int]] = {

      import cats.data.OptionT

      val result = for {
        a <- OptionT(parseNumber(a))
        b <- OptionT(parseNumber(b))
        c <- OptionT(parseNumber(c))
      } yield a + b + c

      result.value
    }

    // This approach doesn't force OptionT on other users' code:
    val result1 = addAll("1", "2", "3")
    println(result1) // WriterT((List(Read 1, Read 2, Read 3),Some(6)))

    val result2 = addAll("1", "a", "3")
    println(result2) // WriterT((List(Read 1, Failed on a),None))
  }

  println("-----")
}
