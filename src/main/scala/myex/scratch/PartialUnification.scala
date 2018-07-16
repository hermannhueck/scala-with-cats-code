package myex.scratch

import scala.language.higherKinds

// compiles only with -Ypartial-unification enabled
//
// great explanation by Daniel Spiewak at:
// https://gist.github.com/djspiewak/7a81a395c461fd3a09a6941d4cd040f2

object PartialUnification extends App {

  def foo[F[_], A](fa: F[A]): String = fa.toString
  foo { x: Int => x * 2 }
}
