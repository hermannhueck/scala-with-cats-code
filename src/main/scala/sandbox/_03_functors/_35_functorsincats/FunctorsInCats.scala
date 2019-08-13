package sandbox._03_functors._35_functorsincats

import scala.language.higherKinds
import cats.Functor
import cats.instances.list._   // for Functor
import cats.instances.option._ // for Functor
import cats.instances.function._ // for Functor
import cats.syntax.functor._     // for map

object FunctorsInCats extends App {

  println("---")

  val list1 = List(1, 2, 3)
  println(list1)

  val list2 = Functor[List].map(list1)(_ * 2)
  println(list2)

  val option1 = Option(123)
  println(option1)

  val option2 = Functor.apply[Option].map(option1)(_.toString)
  println(option2)

  println("---")

  // Functor also provides the lift method, which converts a function of type A => B
  // to one that operates over a functor and has type F[A] => F[B]:

  val func = (x: Int) => x + 1

  val liftedFunc = Functor[Option].lift(func)

  println(liftedFunc(Option(1)))

  println("---")

  val func1 = (a: Int) => a + 1
  val func2 = (a: Int) => a * 2
  val func3 = (a: Int) => s"$a!"
  val func4 = func1.map(func2).map(func3)

  println(func4(123))

  println("---")

  def doMath[F[_]](start: F[Int])(implicit functor: Functor[F]): F[Int] =
    start.map(n => n + 1 * 2)

  println(doMath(Option(20)))
  println(doMath(List(1, 2, 3)))

  // val foo = 10
  // println(new FunctorOps(foo).map(value => value + 1))

  final case class Box[A](value: A)

  implicit val boxFunctor = new Functor[Box] {
    override def map[A, B](fa: Box[A])(f: A => B): Box[B] = Box(f(fa.value))
  }

  val box = Box[Int](123)
  println(box.map(value => value + 1))

  println("---")

  implicit val optionFunctor: Functor[Option] =   // this one already exists
    new Functor[Option] {
      def map[A, B](value: Option[A])(func: A => B): Option[B] =
        value.map(func)
    }

  println(Some(3).map(_*2)) // invokes Option.map
  //println(Some(3).map(_*2)(optionFunctor)) // explicitly invokes optionFunctor's map method

  import scala.concurrent.{Future, ExecutionContext}

  implicit def futureFunctor
  (implicit ec: ExecutionContext): Functor[Future] =
    new Functor[Future] {
      def map[A, B](value: Future[A])(func: A => B): Future[B] =
        value.map(func)
    }


  println("---")
}
