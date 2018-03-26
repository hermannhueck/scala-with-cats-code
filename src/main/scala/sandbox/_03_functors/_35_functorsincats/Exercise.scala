package sandbox._03_functors._35_functorsincats

import cats.Functor
import cats.syntax.functor._     // for map

object Exercise extends App {

  println("---")

  sealed trait Tree[+A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]
  final case class Leaf[A](value: A) extends Tree[A]

  implicit val treeFunctor = new Functor[Tree] {
    override def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
      case Leaf(value) => Leaf(f(value))
      case Branch(left, right) => Branch(map(left)(f), map(right)(f))
    }
  }

  val tree = Branch(Leaf(1), Branch(Leaf(2), Leaf(3)))
  println(tree)

  // val mapped = tree.map(_ * 2) // compile error: doesn't find map for Leaf and Branch, because Functor is invariant

  object Tree {

    def branch[A](left: Tree[A], right: Tree[A]): Tree[A] =
      Branch(left, right)

    def leaf[A](value: A): Tree[A] =
      Leaf(value)
  }

  import Tree._

  val tree2 = branch(leaf(1), branch(leaf(2), leaf(3)))
  println(tree2)

  val mapped = tree2.map(_ * 2)
  println(mapped)

  println(leaf(100).map(_ * 2))
  println(branch(leaf(10), leaf(20)).map(_ * 2))

  println("---")
}
