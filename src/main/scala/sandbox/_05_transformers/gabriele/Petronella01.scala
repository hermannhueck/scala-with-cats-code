package sandbox._05_transformers.gabriele

import cats.Functor
import cats.instances.future._
import cats.instances.list._

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.concurrent.{Await, Future}

object Petronella01 extends App {

  println("--- composing functors")

  def mapFL[A, B](fl: Future[List[A]])(f: A => B): Future[List[B]] =
    fl.map(_.map(f))

  val fl0 = Future(List(1, 2, 3))
  val fl1 = mapFL(fl0)(_ + 1)
  println(Await.result(fl1, 3.seconds))

  val futureFunctor: Functor[Future] = new Functor[Future] {
    override def map[A, B](fa: Future[A])(f: A => B): Future[B] = fa.map(f)
  }
  val listFunctor: Functor[List] = new Functor[List] {
    override def map[A, B](fa: List[A])(f: A => B): List[B] = fa.map(f)
  }
  val futureListFunctor = futureFunctor compose listFunctor
  val fl2 = futureListFunctor.map(fl0)(_ + 1)
  println(Await.result(fl2, 3.seconds))

  val futureListFunctor2 = Functor[Future] compose Functor[List]
  val fl3 = futureListFunctor2.map(fl0)(_ + 1)
  println(Await.result(fl3, 3.seconds))

  println("-----")
}
