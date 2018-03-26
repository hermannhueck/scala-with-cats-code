package sandbox._04_monads._410_custom

import cats.Monad

//import scala.annotation.tailrec

object TreeMonad extends App {

  println("--- 4.10.1 Exercise: Branching out Further with Monads")

  sealed trait Tree[+A]
  final case class Leaf[A](value: A) extends Tree[A]
  final case class Branch[A](left: Tree[A], right: Tree[A]) extends Tree[A]

  def leaf[A](value: A): Tree[A] = Leaf(value)

  def branch[A](left: Tree[A], right: Tree[A]): Tree[A] = Branch(left, right)


  implicit val treeMonad = new Monad[Tree] {

    def pure[A](value: A): Tree[A] = Leaf(value)

    def flatMap[A, B](tree: Tree[A])(fn: A => Tree[B]): Tree[B] = tree match {
      case Leaf(x) => fn(x)
      case Branch(left, right) => Branch(flatMap(left)(fn), flatMap(right)(fn))
    }

    // this tailRecM is not tail recursive.
    // for the tail recursive version see the solution
    // in chapter 12.4.12 in "Scala with Cats"
    def tailRecM[A, B](a: A)(f: A => Tree[Either[A, B]]): Tree[B] =
      f(a) match {
        case Leaf(Left(a1)) => tailRecM(a1)(f)
        case Leaf(Right(b)) => Leaf(b)
        case Branch(l, r) =>
          Branch(
            flatMap(l) {
              case Left(l)  => tailRecM(l)(f)
              case Right(l) => pure(l)
            },
            flatMap(r) {
              case Left(r)  => tailRecM(r)(f)
              case Right(r) => pure(r)
            }
          )
      }
  }

  import cats.syntax.functor._ // for map
  import cats.syntax.flatMap._ // for flatMap

  val t1 = branch(leaf(100), leaf(200)).
    flatMap(x => branch(leaf(x - 1), leaf(x + 1)))
  // Branch(Branch(Leaf(99),Leaf(101)),Branch(Leaf(199),Leaf(201)))
  println(t1)

  val t2 = for {
    a <- branch(leaf(100), leaf(200))
    b <- branch(leaf(a - 10), leaf(a + 10))
    c <- branch(leaf(b - 1), leaf(b + 1))
  } yield c
  // Branch(Branch(Branch(Leaf(89),Leaf(91)),Branch(Leaf(109),Leaf(111))),Branch(Branch(Leaf(189),Leaf(191)),Branch(Leaf(209),Leaf(211))))
  println(t2)

  println("---")
}
