package exkittens

import hutil.syntax.pipe._

import cats.implicits._
import cats.sequence._

object Ex03Sequence extends hutil.App {

  val f1: String => Int    = (_: String).length
  val f2: String => String = (_: String).reverse
  val f3: String => Float  = (_: String).toFloat

  val f = sequence(f1, f2, f3)
  // f: String => shapeless.::[Int,shapeless.::[String,shapeless.::[Float,shapeless.HNil]]] = <function1>

  f("42.0") | println

  case class MyCase(a: Int, b: String, c: Float)

  val myGen = sequenceGeneric[MyCase]

  val fc = myGen(a = f1, b = f2, c = f3)

  fc("42.0") | println
}
