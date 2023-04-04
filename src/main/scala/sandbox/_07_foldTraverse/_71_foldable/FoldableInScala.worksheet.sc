// ----- 7.1.1 Folds and Folding

def show[A](list: List[A]): String =
  list.foldLeft("nil")((accum, item) => s"$item then $accum")

show(Nil)
show(List(1, 2, 3))

List(1, 2, 3).foldLeft(0)(_ + _)
List(1, 2, 3).foldRight(0)(_ + _)

List(1, 2, 3).foldLeft(0)(_ - _)
List(1, 2, 3).foldRight(0)(_ - _)

// ----- 7.1.2 Exercise: Reflecting on Folds

List(1, 2, 3).foldLeft(List.empty[Int])((a, i) => i :: a)
// == List.resverse
List(1, 2, 3).foldRight(List.empty[Int])((i, a) => i :: a)
// == List

// ----- 7.1.3 Exercise: Scaf-fold-ing Other Methods

val l = List(1, 2, 3, 4, 5)

// -- filter

def filter[A](l: List[A])(p: A => Boolean): List[A] =
  l.foldRight(List.empty[A]) { (elem, acc) =>
    if (p(elem)) elem :: acc else acc
  }

def even(x: Int) = x % 2 == 0
l.filter(even)
filter(l)(even)
assert(l.filter(even) == filter(l)(even))

// -- map

def map[A, B](l: List[A])(f: A => B): List[B] =
  l.foldRight(List.empty[B]) { (elem, acc) =>
    f(elem) :: acc
  }

def square(x: Int) = x * x
l.map(square)
map(l)(square)
assert(l.map(square) == map(l)(square))

// -- flatMap

def flatMap[A, B](l: List[A])(f: A => List[B]): List[B] =
  l.foldRight(List.empty[B]) { (elem, acc) =>
    f(elem) ::: acc
  }

def squareList(x: Int) = List(x * x)
l.flatMap(squareList)
flatMap(l)(squareList)
assert(l.flatMap(squareList) == flatMap(l)(squareList))
flatMap(List(1, 2, 3))(a => List(a, a * 10, a * 100))

// -- sumOfInts

def sumOfInts(ints: List[Int]): Int =
  ints.fold(0)(_ + _)
l.sum
sumOfInts(l)
assert(l.sum == sumOfInts(l))

// -- sumWithNumeric

import scala.math.Numeric

def sumWithNumeric[A](list: List[A])(implicit numeric: Numeric[A]): A =
  list.foldRight(numeric.zero)(numeric.plus)

sumWithNumeric(List(1, 2, 3))

// -- sumWithMonoid

import cats.Monoid

def sumWithMonoid[A](list: List[A])(implicit monoid: Monoid[A]): A =
  list.foldRight(monoid.empty)(monoid.combine)

import cats.instances.int._ // for Monoid

sumWithMonoid(List(1, 2, 3))
