package myex.applicative

import scala.language.higherKinds

import cats._, cats.data._, cats.implicits._

object DeducingMapN extends App {

  println("\n===== Deducing TupleN#mapN")

  val add: (Int, Int) => Int = _ + _
  val addCurried: Int => Int => Int = add.curried


  println("\n===== Monadic processing first ...")

  println("----- uncurriedMonadicComputeOptionInt:")
  def uncurriedMonadicComputeOptionInt(v1: Option[Int], v2: Option[Int], f: (Int, Int) => Int): Option[Int] =
    for {
      i1 <- v1
      i2 <- v2
    } yield f(i1, i2)

  println(monadicComputeOptionInt(Option(1), Option(2), addCurried))

  println("----- monadicComputeOptionInt:")
  def monadicComputeOptionInt(v1: Option[Int], v2: Option[Int], f: Int => Int => Int): Option[Int] =
    for {
      i1 <- v1
      i2 <- v2
    } yield f(i1)(i2)

  println(monadicComputeOptionInt(Option(1), Option(2), addCurried))

  println("----- monadicComputeListInt:")
  def monadicComputeListInt(v1: List[Int], v2: List[Int], f: Int => Int => Int): List[Int] =
    for {
      i1 <- v1
      i2 <- v2
    } yield f(i1)(i2)

  println(monadicComputeListInt(List(1), List(2), addCurried))
  println(monadicComputeListInt(List(1, 2), List(10, 20), addCurried))

  println("----- monadicComputeFInt:")
  def monadicComputeFInt[F[_]: Monad](v1: F[Int], v2: F[Int], f: Int => Int => Int): F[Int] =
    for {
      i1 <- v1
      i2 <- v2
    } yield f(i1)(i2)

  println(monadicComputeFInt(Option(1), Option(2), addCurried))
  println(monadicComputeFInt(List(1), List(2), addCurried))
  println(monadicComputeFInt(List(1, 2), List(10, 20), addCurried))

  println("----- desugaredMonadicComputeFInt:")
  def desugaredMonadicComputeFInt[F[_]: Monad](v1: F[Int], v2: F[Int], f: Int => Int => Int): F[Int] =
    v1 flatMap { x => v2 map { y => f(x)(y) } }

  println(desugaredMonadicComputeFInt(Option(1), Option(2), addCurried))
  println(desugaredMonadicComputeFInt(List(1), List(2), addCurried))
  println(desugaredMonadicComputeFInt(List(1, 2), List(10, 20), addCurried))


  println("\n===== Achiving the same thing with Applicative ...")

  println("----- computeOptionInt:")
  def computeOptionInt(v1: Option[Int], v2: Option[Int], f: Int => Int => Int): Option[Int] =
    f.pure[Option] ap v1 ap v2

  println(computeOptionInt(Option(1), Option(2), addCurried))

  println("----- computeListInt:")
  def computeListInt(v1: List[Int], v2: List[Int], f: Int => Int => Int): List[Int] =
    f.pure[List] ap v1 ap v2

  println(computeListInt(List(1), List(2), addCurried))
  println(computeListInt(List(1, 2), List(10, 20), addCurried))

  println("----- computeFInt:")
  def computeFInt[F[_]: Applicative](v1: F[Int], v2: F[Int], f: Int => Int => Int): F[Int] =
    f.pure[F] ap v1 ap v2

  println(computeFInt(Option(1), Option(2), addCurried))
  println(computeFInt(List(1), List(2), addCurried))
  println(computeFInt(List(1, 2), List(10, 20), addCurried))

  println("----- computeFAB:")
  def computeFAB[F[_]: Applicative, A, B, Z](v1: F[A], v2: F[B], f: (A, B) => Z): F[Z] =
    f.curried.pure[F] ap v1 ap v2

  println(computeFAB(Option(1), Option(2), add))
  println(computeFAB(List(1, 2), List(10, 20), add))

  def computeT2FAB[F[_]: Applicative, A, B, Z](t2: (F[A], F[B]))(f: (A, B) => Z): F[Z] =
    t2 match {
      case (fst, snd) => f.curried.pure[F] ap fst ap snd
    }

  println("----- computeT2FAB:")
  println(computeT2FAB((Option(1), Option(2)))(add))
  println(computeT2FAB((List(1, 2), List(10, 20)))(add))

  println("----- myMap2:")
  def myMap3[F[_]: Applicative, A, B, Z](t2: (F[A], F[B]))(f: (A, B) => Z): F[Z] =
    computeT2FAB(t2)(f)

  println(myMap3((Option(1), Option(2)))(_ + _))
  println(myMap3((List(1, 2), List(10, 20)))(_ + _))

  println("----- myMap3:")
  def myMap3[F[_]: Applicative, A, B, C, Z](t3: (F[A], F[B], F[C]))(f: (A, B, C) => Z): F[Z] =
    t3 match {
      case (fst, snd, trd) => f.curried.pure[F] ap fst ap snd ap trd
    }

  println(myMap3((Option(1), Option(2), Option(3)))(_ + _ + _))
  println(myMap3((List(1, 2), List(10, 20), List(100, 200)))(_ + _ + _))

  println("\n----- 'myMap4', 'myMap5', 'myMap6' .. 'myMap22' can easily be implemented for Tuple4, Tuple5, Tuple6 .. Tuple22")
  println("----- That is boring boilerplate. Cats provides 'Applicative.map4', 'Applicative.map5' .. 'Applicative.map22' for us.")
  println("----- But it is more convenient to use 'mapN' instead.")

  println("\n----- Tuple2#myMapN & Tuple2#myTupled:")
  implicit class RichTuple2[F[_]: Applicative, A, B](t2: (F[A], F[B])) {
    def myMapN[Z](f: (A, B) => Z): F[Z] = myMap3(t2)(f) // 'mapN' in Cats
    def myTupled: F[(A, B)] = myMap3(t2)((_, _)) // 'tupled' in Cats
  }

  println((Option(1), Option(2)) myMapN (_ + _))
  println((List(1, 2), List(10, 20)) myMapN (_ + _))
  println((Option(1), Option(2)).myTupled)
  println((List(1, 2), List(10, 20)).myTupled)

  println("----- Tuple3#myMapN & Tuple3#myTupled:")
  implicit class RichTuple3[F[_]: Applicative, A, B, C](t3: (F[A], F[B], F[C])) {
    def myMapN[Z](f: (A, B, C) => Z): F[Z] = myMap3(t3)(f) // 'mapN' in Cats
    def myTupled: F[(A, B, C)] = myMap3(t3)((_, _, _)) // 'tupled' in Cats
  }

  println((Option(1), Option(2), Option(3)) myMapN (_ + _ + _))
  println((List(1, 2), List(10, 20), List(100, 200)) myMapN (_ + _ + _))
  println((Option(1), Option(2), Option(3)).myTupled)
  println((List(1, 2), List(10, 20), List(100, 200)).myTupled)

  println("\n----- 'myMapN' & 'myTupled' can easily be implemented for Tuple4, Tuple5, Tuple6 .. Tuple22")
  println("----- But that is boring boilerplate. Cats provides 'mapN' & 'tupled' for us.")


  println("\n===== Using 'mapN' and 'tupled' from Cats ...")

  println("----- ... for Tuple2:")

  println((Option(1), Option(2)) mapN (_ + _))
  println((List(1, 2), List(10, 20)) mapN (_ + _))
  println((Option(1), Option(2)).tupled)
  println((List(1, 2), List(10, 20)).tupled)

  println("----- ... for Tuple3:")

  println((Option(1), Option(2), Option(3)) mapN (_ + _ + _))
  println((List(1, 2), List(10, 20), List(100, 200)) mapN (_ + _ + _))
  println((Option(1), Option(2), Option(3)).tupled)
  println((List(1, 2), List(10, 20), List(100, 200)).tupled)

  println("\n-----\n")
}
