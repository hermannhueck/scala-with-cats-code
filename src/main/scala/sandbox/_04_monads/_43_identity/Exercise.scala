package sandbox._04_monads._43_identity

import cats.Id

object Exercise extends App {

  println("---")

  def pure[A](value: A): Id[A] = value

  def map[A, B](initial: Id[A])(func: A => B): Id[B] = func(initial)

  def flatMap[A, B](initial: Id[A])(func: A => Id[B]): Id[B] = func(initial)

  println(pure(123)) // --> 123
  println(map(123)(_ * 2)) // --> 246
  println(flatMap(123)(_ * 2)) // --> 246

  println("---")
}
