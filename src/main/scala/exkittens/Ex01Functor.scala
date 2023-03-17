package exkittens

import hutil.syntax.pipe._

import cats.implicits._, cats._, cats.derived._

object Ex01Functor extends hutil.App {

  case class Cat[Food](food: Food, foods: List[Food])

  val cat = Cat(1, List(2, 3))
  cat | println

  implicit val fc: Functor[Cat] = {
    import auto.functor._
    semiauto.functor
  }

  cat.map(_ + 1) | println
}
