import cats.Monad

//import scala.annotation.tailrec

println("--- 4.10.1 Exercise: Branching out Further with Monads")

sealed trait Tree[+A]
final case class Leaf[A](value: A)                        extends Tree[A]
final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

object Tree {

  def leaf[A](value: A): Tree[A] =
    Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
    Branch(left, right)
}

import Tree._

implicit val treeMonad: Monad[Tree] = new Monad[Tree] {

  def pure[A](value: A): Tree[A] = Leaf(value)

  def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
    case Leaf(x)             =>
      fn(x)
    case Branch(left, right) =>
      Branch(flatMap(left)(fn), flatMap(right)(fn))
  }

  // new solution (not tail recursive)
  // for the tail recursive version see the solution
  // in chapter 12.4.12 in "Scala with Cats"
  def tailRecM[A, B](a: A)(func: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(func(a)) {
      case Left(value)  =>
        tailRecM(value)(func)
      case Right(value) =>
        Leaf(value)
    }

  // old solution (not tail recursive)
  // def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
  //   f(a) match {
  //     case Leaf(Right(b))      => Leaf(b)
  //     case Leaf(Left(value))   => tailRecM(value)(f)
  //     case Branch(left, right) =>
  //       Branch(
  //         flatMap(left) {
  //           case Right(l) => pure(l)
  //           case Left(l)  => tailRecM(l)(f)
  //         },
  //         flatMap(right) {
  //           case Right(r) => pure(r)
  //           case Left(r)  => tailRecM(r)(f)
  //         }
  //       )
  //   }
}

import cats.syntax.functor._ // for map
import cats.syntax.flatMap._ // for flatMap

val t1 =
  branch(leaf(100), leaf(200))
    .flatMap(x => branch(leaf(x - 1), leaf(x + 1)))

t1

val t2 = for {
  a <- branch(leaf(100), leaf(200))
  b <- branch(leaf(a - 10), leaf(a + 10))
  c <- branch(leaf(b - 1), leaf(b + 1))
} yield c

t2
