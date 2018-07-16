package myex.applicative

import scala.language.higherKinds

import cats._, cats.data._, cats.implicits._

object ApplicativeProgrammingWithEffects extends App {

  println("\n----- Examples from McBride & Patterson: Applicative Programming with Effects")

  // McBride & Patterson, page 1

  def listSequenceM0[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => Some(Nil)
    case opt :: opts =>
      for {
        x <- opt
        xs <- listSequenceM0(opts)
      } yield x :: xs
  }

  def listSequenceM1[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => Monad[Option].pure(List.empty[A])
    case opt :: opts =>
      opt flatMap { x => listSequenceM1(opts) map { xs => x :: xs } }
  }

  def listSequenceM2[F[_]: Monad, A](loa: List[F[A]]): F[List[A]] = loa match {
    case Nil => Monad[F].pure(List.empty[A])
    case opt :: opts =>
      opt flatMap { x => listSequenceM2(opts) map { xs => x :: xs } }
  }

  // McBride & Patterson, page 2

  def listSequenceA0[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => Some(Nil)
    case opt :: opts =>
      val consCurried = ((a: A, as: List[A]) => a :: as).curried
      val fopt: Option[List[A] => List[A]] = opt map consCurried
      Applicative[Option].ap(fopt)(listSequenceA0(opts))
  }

  def listSequenceA1[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => Some(Nil)
    case opt :: opts =>
      val consCurried: A => List[A] => List[A] = a => as => a :: as
      opt map consCurried ap listSequenceA1(opts)
  }

  def listSequenceA2[A](loa: List[Option[A]]): Option[List[A]] = loa match {
    case Nil => List.empty[A].pure[Option]
    case opt :: opts =>
      val consCurried: A => List[A] => List[A] = a => as => a :: as
      consCurried.pure[Option] ap opt ap listSequenceA2(opts)
  }

  def listSequenceA3[F[_]: Applicative, A](lfa: List[F[A]]): F[List[A]] = lfa match {
    case Nil => List.empty[A].pure[F]
    case fa :: fas =>
      val cons = (a: A, as: List[A]) => a :: as
      cons.curried.pure[F] <*> fa <*> listSequenceA3(fas)
  }

  println("-----\n")
}
