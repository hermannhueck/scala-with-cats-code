package sandbox._00_hello_cats

import cats.instances.string._
import cats.syntax.semigroup._

object Main extends App {
  println()
  println("Hello " |+| "Cats!")
  println()
}
