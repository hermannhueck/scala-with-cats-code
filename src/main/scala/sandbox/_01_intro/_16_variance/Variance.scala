package sandbox._01_intro._16_variance

import sandbox._01_intro._11_typeclass.json_improved.lib.Json

object Variance {

  sealed trait Shape
  case class Circle(radius: Double) extends Shape
  val circles: List[Circle] = ???
  val shapes: List[Shape]   = circles

  trait JsonWriter[-A] {
    def write(value: A): Json
  }

  val shape: Shape   = ???
  val circle: Circle = ???

  val shapeWriter: JsonWriter[Shape]   = ???
  val circleWriter: JsonWriter[Circle] = ???

  def format[A](value: A, writer: JsonWriter[A]): Json =
    writer.write(value)

  format(circle, circleWriter)
  // format(shape, circleWriter) // compile error
  format(circle, shapeWriter)
  format(shape, shapeWriter)
}
