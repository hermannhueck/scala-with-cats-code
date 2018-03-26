package sandbox._03_functors._37_contrainvariantincats

// import cats.Invariant
import cats.Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.invariant._ // for imap
import cats.syntax.semigroup._ // for |+|

object InvariantInCats extends App {

  println("---")

  implicit val symbolMonoid: Monoid[Symbol] =
    Monoid[String].imap(Symbol.apply)(_.name)

  println(Monoid[Symbol].empty)
  // res5: Symbol = '

  println('a |+| 'few |+| 'words)
  // res6: Symbol = 'afewwords

  println("---")
}
