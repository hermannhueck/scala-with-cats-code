package sandbox._04_monads._410_custom

object OptionMonad extends App {

  println("--- 4.10 Defining Custom Monads")

  import cats.Monad
  import scala.annotation.tailrec

  implicit val optionMonad: Monad[Option] = new Monad[Option] {

    def pure[A](value: A): Option[A] = Some(value)

    def flatMap[A, B](opt: Option[A])(fn: A => Option[B]): Option[B] =
      opt flatMap fn

    @tailrec
    def tailRecM[A, B](a: A)(fn: A => Option[Either[A, B]]): Option[B] =
      fn(a) match {
        case None           => None
        case Some(Right(b)) => Some(b)
        case Some(Left(a1)) => tailRecM(a1)(fn)
      }
  }

  val opt = optionMonad.pure("foo")

  val result = opt.flatMap(s => Option((s.toUpperCase, s.length)))

  println(opt)
  println(result)

  println("---")
}
