import java.util.concurrent.Future
import scala.util.Try
import cats.Foldable

// ----- 7.1.4 Foldable in Cats

import cats.Foldable
import cats.instances.list._ // for Foldable

val ints = List(1, 2, 3)

Foldable[List].foldLeft(ints, 0)(_ + _)

import cats.instances.option._ // for Foldable

val maybeInt = Option(123)

Foldable[Option].foldLeft(maybeInt, 10)(_ * _)

// not supported in Cats 2.0
// ----- 7.1.4.1 Folding Right

import cats.Eval
import cats.Foldable

def bigData = (1 to 100000).to(LazyList)

bigData.foldRight(0L)(_ + _) // foldRight of LazyList is not stack safe
// java.lang.StackOverflowError ...
// has been fixed in the standard library in Scala 2.13

import cats.instances.lazyList._ // for Foldable

val eval: Eval[Long] =
  Foldable[LazyList].foldRight(bigData, Eval.now(0L)) { (num, eval) =>
    eval.map(_ + num)
  }

eval.value

// ----- Stack Safety in the Standard Library

(1 to 100000).to(LazyList).foldRight(0L)(_ + _) // stack safe
(1 to 100000).toList.foldRight(0L)(_ + _)       // stack safe
(1 to 100000).toVector.foldRight(0L)(_ + _)     // stack safe

// ----- 7.1.4.2 Folding with Monoids

import cats.instances.option._
import cats.instances.list._

Foldable[Option].nonEmpty(Option(42))
Foldable[List].find(List(1, 2, 3))(_ % 2 == 0)

import cats.instances.int._ // for Monoid

Foldable[List].combineAll(List(1, 2, 3))

import cats.instances.string._ // for Monoid

Foldable[List].foldMap(List(1, 2, 3))(_.toString)

import cats.instances.vector._ // for Monoid

val ints2 = List(Vector(1, 2, 3), Vector(4, 5, 6))

(Foldable[List] compose Foldable[Vector]).combineAll(ints2)

// ----- 7.1.4.3 Syntax for Foldable

import cats.instances.list._
import cats.instances.int._    // for Monoid
import cats.instances.string._ // for Monoid
import cats.syntax.foldable._  // for combineAll and foldMap

List(1, 2, 3).combineAll
List(1, 2, 3).foldMap(_.toString)

// --- Checking Foildable instnces

Foldable[List]
Foldable[Option]
Foldable[Either[String, *]]
Foldable[Try]
// Foldable[Future]
