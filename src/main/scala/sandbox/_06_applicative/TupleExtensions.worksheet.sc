implicit final class Tuple2OptionExtensions[A, B](val tupleOpts: (Option[A], Option[B])) {

  def mapN[Z](f: (A, B) => Z): Option[Z] =
    (tupleOpts._1, tupleOpts._2) match {
      case (Some(a), Some(b)) => Some(f(a, b))
      case _                  => None
    }

  def tupled: Option[(A, B)] =
    mapN((a, b) => (a, b))
}

(Option(1), Option(2)).mapN(_ + _)
(Option(1), Option.empty[Int]).mapN(_ + _)

(Option(1), Option(2)).tupled
(Option(1), Option.empty[Int]).tupled

implicit final class Tuple3OptionExtensions[A, B, C](val tupleOpts: (Option[A], Option[B], Option[C])) {

  def mapN[Z](f: (A, B, C) => Z): Option[Z] =
    (tupleOpts._1, tupleOpts._2, tupleOpts._3) match {
      case (Some(a), Some(b), Some(c)) => Some(f(a, b, c))
      case _                           => None
    }

  def tupled: Option[(A, B, C)] =
    mapN((a, b, c) => (a, b, c))
}

(Option(1), Option(2), Option(3)).mapN(_ + _ + _)
(Option(1), Option.empty[Int], Option(3)).mapN(_ + _ + _)

(Option(1), Option(2), Option(3)).tupled
(Option(1), Option.empty[Int], Option(3)).tupled

implicit final class Tuple2ListExtensions[A, B](val tupleLists: (List[A], List[B])) {

  def mapN[Z](f: (A, B) => Z): List[Z] =
    for {
      a <- tupleLists._1
      b <- tupleLists._2
    } yield f(a, b)

  def tupled: List[(A, B)] =
    mapN((a, b) => (a, b))
}

(List(1, 2, 3), List(4, 5)).mapN(_ + _)
(List(1, 2, 3), List.empty[Int]).mapN(_ + _)

(List(1, 2, 3), List(4, 5)).tupled
(List(1, 2, 3), List.empty[Int]).tupled

implicit final class Tuple3ListExtensions[A, B, C](val tupleLists: (List[A], List[B], List[C])) {

  def mapN[Z](f: (A, B, C) => Z): List[Z] =
    for {
      a <- tupleLists._1
      b <- tupleLists._2
      c <- tupleLists._3
    } yield f(a, b, c)

  def tupled: List[(A, B, C)] =
    mapN((a, b, c) => (a, b, c))
}

(List(1, 2, 3), List(4, 5), List(6, 7)).mapN(_ + _ + _)
(List(1, 2, 3), List.empty[Int], List(6, 7)).mapN(_ + _ + _)

(List(1, 2, 3), List(4, 5), List(6, 7)).tupled
(List(1, 2, 3), List.empty[Int], List(6, 7)).tupled
