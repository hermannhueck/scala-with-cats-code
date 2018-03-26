package sandbox._03_functors._37_contrainvariantincats

import cats.Contravariant
import cats.Show
import cats.instances.string._
import cats.syntax.contravariant._ // for contramap

object ContravariantInCats extends App {

  println("---")

  val showString = Show[String]

  val showSymbol = Contravariant[Show].
    contramap(showString)((sym: Symbol) => s"'${sym.name}")

  println(showSymbol.show('dave))

  println(showString.contramap[Symbol](_.name).show('dave))

  println("---")
}
