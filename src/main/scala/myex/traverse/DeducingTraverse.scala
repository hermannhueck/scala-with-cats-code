package myex.traverse

import scala.language.postfixOps
import scala.language.higherKinds

import scala.concurrent.{Await, Future}
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global

object DeducingTraverse extends App {

  val hostnames: List[String] = List(
    "alpha.example.com",
    "beta.example.com",
    "gamma.demo.com"
  )

  def getUptime(hostname: String): Future[Int] =
    Future(hostname.length * 60) // just for demonstration

  {
    println("\n----- Using Future.traverse")

    val allUptimes: Future[List[Int]] =
      Future.traverse(hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Using Traverse[List].traverse")

    import cats.Traverse
    import cats.instances.list._
    import cats.instances.future._

    val allUptimes: Future[List[Int]] =
      Traverse[List].traverse[Future, String, Int](hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))

    // --- with syntax support
    import cats.syntax.traverse._

    val allUptimes2: Future[List[Int]] = hostnames traverse getUptime

    println(Await.result(allUptimes2, 1 second))
  }

  {
    println("\n----- Using Future.traverse is semantically the same as mapping and then invoking Future.sequence")

    val allUptimes: Future[List[Int]] = {
      val listFutUptimes: List[Future[Int]] = hostnames.map(getUptime)
      val futListUptimes: Future[List[Int]] = Future.sequence(listFutUptimes)
      futListUptimes
    }

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Using Traverse[List].sequence")

    import cats.Traverse
    import cats.instances.list._
    import cats.instances.future._

    val allUptimes: Future[List[Int]] = {
      val listFutUptimes: List[Future[Int]] = hostnames.map(getUptime)
      val futListUptimes: Future[List[Int]] = Traverse[List].sequence(listFutUptimes)
      futListUptimes
    }

    println(Await.result(allUptimes, 1 second))

    // --- with syntax support
    import cats.syntax.traverse._

    val allUptimes2: Future[List[Int]] = hostnames.map(getUptime).sequence

    println(Await.result(allUptimes2, 1 second))
  }

  {
    println("\n----- Step 1: We implement sequencing ourselves with List.foldLeft and a for comprehension with Future context")

    val allUptimes: Future[List[Int]] = {
      val listFutUptimes: List[Future[Int]] = hostnames.map(getUptime)
      val futListUptimes: Future[List[Int]] =
        listFutUptimes.foldLeft(Future(List.empty[Int])) {
          (acc: Future[List[Int]], elem: Future[Int]) =>
            for {
              list <- acc
              value <- elem
            } yield list :+ value
        }
      futListUptimes
    }

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 2: We replace the for comprehension by flatMap and map")

    val allUptimes: Future[List[Int]] = {
      val listFutUptimes: List[Future[Int]] = hostnames.map(getUptime)
      val futListUptimes: Future[List[Int]] =
        listFutUptimes.foldLeft(Future(List.empty[Int])) {
          (acc: Future[List[Int]], elem: Future[Int]) =>
            acc.flatMap {
              list => elem.map {
                value => list :+ value
              }
            }
        }
      futListUptimes
    }

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 3: We extract sequencing into a separate method")

    def mySequence(listFutUptimes: List[Future[Int]]): Future[List[Int]] = {
      listFutUptimes.foldLeft(Future(List.empty[Int])) {
        (acc: Future[List[Int]], elem: Future[Int]) =>
          acc.flatMap {
            list => elem.map {
              value => list :+ value
            }
          }
      }
    }

    val allUptimes: Future[List[Int]] = {
      val listFutUptimes: List[Future[Int]] = hostnames.map(getUptime)
      mySequence(listFutUptimes)
    }

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 4: We replace the invocations on List and Future by direct invocations on the typeclasses: Applicative, Foldable, Monad, Functor")

    import cats.{Applicative, Foldable, Monad, Functor}
    import cats.instances.list._
    import cats.instances.future._

    def mySequence(listFutInt: List[Future[Int]]): Future[List[Int]] = {
      Foldable[List].foldLeft(listFutInt, Applicative[Future].pure(List.empty[Int])) {
        (acc: Future[List[Int]], elem: Future[Int]) => Applicative[Future].product(acc, elem)
          Monad[Future].flatMap(acc) {
            list => Functor[Future].map(elem) {
              value => list :+ value
            }
          }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 5: We don't need a Monad[Future]. Applicative[Future].product (followed by map) is sufficient.")

    import cats.{Applicative, Foldable, Functor}
    import cats.instances.list._
    import cats.instances.future._

    def mySequence(listFutInt: List[Future[Int]]): Future[List[Int]] = {
      Foldable[List].foldLeft(listFutInt, Applicative[Future].pure(List.empty[Int])) {
        (acc: Future[List[Int]], elem: Future[Int]) =>
          val prod: Future[(List[Int], Int)] = Applicative[Future].product(acc, elem)
          Functor[Future].map(prod) { case (li, i) => li :+ i }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 6: 'mapN' is a bit neater than 'product', but it does the same thing")

    import cats.{Applicative, Foldable}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence(listFutInt: List[Future[Int]]): Future[List[Int]] = {
      Foldable[List].foldLeft(listFutInt, Applicative[Future].pure(List.empty[Int])) {
        (acc: Future[List[Int]], elem: Future[Int]) =>
          (acc, elem).mapN { case (li, i) => li :+ i }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 7: We abstract away the Future to the type constructor G[_]")

    import cats.{Applicative, Foldable}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence[G[_]: Applicative](listFutInt: List[G[Int]]): G[List[Int]] = {
      Foldable[List].foldLeft(listFutInt, Applicative[G].pure(List.empty[Int])) {
        (acc: G[List[Int]], elem: G[Int]) =>
          (acc, elem).mapN { case (li, i) => li :+ i }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 8: We replace 'li :+ i' by list concatenation with ++: 'li ++ Applicative[List].pure(i)'")

    import cats.{Applicative, Foldable}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence[F[_]: Applicative](listFutInt: List[F[Int]]): F[List[Int]] = {

      Foldable[List].foldLeft(listFutInt, Applicative[F].pure(List.empty[Int])) {
        (acc: F[List[Int]], elem: F[Int]) =>
          (acc, elem).mapN { case (li, i) => li ++ Applicative[List].pure(i) }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 9: As ++ is the monoidal operation for lists, we can now use Monoid.combine for this operation")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence[G[_]: Applicative](listFutInt: List[G[Int]]): G[List[Int]] = {

      Foldable[List].foldLeft(listFutInt, Applicative[G].pure(Monoid[List[Int]].empty)) {
        (acc: G[List[Int]], elem: G[Int]) =>
          (acc, elem).mapN { case (li, i) => Monoid[List[Int]].combine(li, Applicative[List].pure(i)) }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 10: We abstract over List, replacing List with F[_]. We need implicit evidence for Monoid[F[Int]].")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence[F[_]: Foldable: Applicative, G[_]: Applicative](listFutInt: F[G[Int]])(implicit ev: Monoid[F[Int]]): G[F[Int]] = {

      Foldable[F].foldLeft(listFutInt, Applicative[G].pure(Monoid[F[Int]].empty)) {
        (acc: G[F[Int]], elem: G[Int]) =>
          (acc, elem).mapN { case (li, i) => Monoid[F[Int]].combine(li, Applicative[F].pure(i)) }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 11: We replace the type Int by the generic type parameter B")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def mySequence[F[_]: Foldable: Applicative, G[_]: Applicative, B](fgb: F[G[B]])(implicit M: Monoid[F[B]]): G[F[B]] = {

      Foldable[F].foldLeft(fgb, Applicative[G].pure(Monoid[F[B]].empty)) {
        (gfb: G[F[B]], gb: G[B]) =>
          (gfb, gb).mapN { case (li, i) => Monoid[F[B]].combine(li, Applicative[F].pure(i)) }
      }
    }

    val allUptimes: Future[List[Int]] = mySequence(hostnames.map(getUptime))

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 12: We rename 'mySequence' to 'myTraverse' and introduce a new type parameter 'A' and a new function parameter 'f: A => G[B]'")
    println("               Mapping outside 'myTraverse' is no longer required. The mapping function is passed in and done inside.")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def myTraverse[F[_]: Foldable: Applicative, G[_]: Applicative, A, B](fa: F[A])
                                                                  (f: A => G[B])
                                                                  (implicit M: Monoid[F[B]]): G[F[B]] = {

      val fgb = Applicative[F].map(fa)(f) // A Functor would be sufficient, but we use the Applicative as we already have it.

      Foldable[F].foldLeft(fgb, Applicative[G].pure(Monoid[F[B]].empty)) {
        (gfb: G[F[B]], gb: G[B]) =>
          (gfb, gb).mapN { case (fb, b) => Monoid[F[B]].combine(fb, Applicative[F].pure(b)) }
      }
    }

    val allUptimes: Future[List[Int]] = myTraverse(hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 13: We draw the invocation of f into the fold. This removes the invocation of map")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.instances.list._
    import cats.instances.future._

    def myTraverse[F[_]: Foldable: Applicative, G[_]: Applicative, A, B](fa: F[A])
                                                                           (f: A => G[B])
                                                                           (implicit M: Monoid[F[B]]): G[F[B]] = {

      Foldable[F].foldLeft(fa, Applicative[G].pure(Monoid[F[B]].empty)) {
        (gfb: G[F[B]], a: A) =>
          (gfb, f(a)).mapN { case (fb, b) => Monoid[F[B]].combine(fb, Applicative[F].pure(b)) }
      }
    }

    val allUptimes: Future[List[Int]] = myTraverse(hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 14: Now lets abbreviate Applicative[F].pure(value) by value.pure[F] from cats.syntax.applicative._")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.syntax.applicative._
    import cats.instances.list._
    import cats.instances.future._

    def myTraverse[F[_]: Foldable: Applicative, G[_]: Applicative, A, B](fa: F[A])
                                                                        (f: A => G[B])
                                                                        (implicit M: Monoid[F[B]]): G[F[B]] =
      Foldable[F].foldLeft(fa, Monoid[F[B]].empty.pure[G]) {
        (gfb: G[F[B]], a: A) =>
          (gfb, f(a)).mapN { case (fb, b) => Monoid[F[B]].combine(fb, b.pure[F]) }
      }

    val allUptimes: Future[List[Int]] = myTraverse(hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 15: Monoid.combine can be replaced with |+| from cats.syntax.semigroup._")

    import cats.{Applicative, Foldable, Monoid}
    import cats.syntax.apply._
    import cats.syntax.applicative._
    import cats.syntax.semigroup._
    import cats.instances.list._
    import cats.instances.future._

    def myTraverse[F[_]: Foldable: Applicative, G[_]: Applicative, A, B](fa: F[A])
                                                                        (f: A => G[B])
                                                                        (implicit M: Monoid[F[B]]): G[F[B]] =
      Foldable[F].foldLeft(fa, Monoid[F[B]].empty.pure[G]) {
        (gfb: G[F[B]], a: A) =>
          (gfb, f(a)).mapN { case (fb, b) => fb |+| b.pure[F] }
      }

    val allUptimes: Future[List[Int]] = myTraverse(hostnames)(getUptime)

    println(Await.result(allUptimes, 1 second))
  }

  {
    println("\n----- Step 16: Instead of using myTraverse for Future and List we can use it for Option and Vector")

    import cats.{Applicative, Foldable, Monoid, Traverse}
    import cats.syntax.apply._
    import cats.syntax.applicative._
    import cats.syntax.semigroup._
    import cats.instances.vector._
    import cats.instances.option._

    def myTraverse[F[_]: Foldable: Applicative, G[_]: Applicative, A, B](fa: F[A])
                                                                        (f: A => G[B])
                                                                        (implicit M: Monoid[F[B]]): G[F[B]] =
      Foldable[F].foldLeft(fa, Monoid[F[B]].empty.pure[G]) {
        (gfb: G[F[B]], a: A) =>
          (gfb, f(a)).mapN { case (fb, b) => fb |+| b.pure[F] }
      }

    val hostnames2: Vector[String] = Vector(
      "alpha.example.com",
      "beta.example.com",
      "gamma.demo.com"
    )

    def getUptime2(hostname: String): Option[Int] =
      Option(hostname.length * 60) // just for demonstration

    val allUptimes: Option[Vector[Int]] = myTraverse(hostnames2)(getUptime2)
    println(allUptimes.get)

    println("               This is quite close to Traverse.traverse")

    val allUptimes2: Option[Vector[Int]] = Traverse[Vector].traverse(hostnames2)(getUptime2)
    println(allUptimes2.get)

  }

  println("\n-----\n")
}
