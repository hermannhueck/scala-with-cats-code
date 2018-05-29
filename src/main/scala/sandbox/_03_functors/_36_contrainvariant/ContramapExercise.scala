package sandbox._03_functors._36_contrainvariant

object ContramapExercise extends App {

  println("---")

  trait Printable[A] {
    self =>

    def format(value: A): String

    def contramap[B](func: B => A): Printable[B] =
      new Printable[B] {
        def format(value: B): String =
          self.format(func(value))
      }
  }

  def format[A](value: A)(implicit p: Printable[A]): String = p.format(value)

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = s"Int with value: $value"
  }

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = "\"" + value + "\""
    }

  implicit val booleanPrintable: Printable[Boolean] = new Printable[Boolean] {
      override def format(value: Boolean): String = if(value) "yes" else "no"
    }

  println(format(15))
  println(format("hello"))
  println(format(true))

  println("---")

  final case class Box[A](value: A)

  // 1. solution based on the Printable[A]
  {
    println("--- Printable for Box[A]: 1. solution based on the Printable[A]")

    implicit def boxPrintable[A](implicit p: Printable[A]): Printable[Box[A]] =
      new Printable[Box[A]] {
        def format(box: Box[A]): String =
          "[" + p.format(box.value) + "] in a Box"
      }

    println(format(Box(15)))
    println(format(Box("hello world")))
    println(format(Box(true)))
  }

  // 2. solution with contramap
  {
    println("--- Printable for Box[A]: 2. solution with contramap")

    implicit def boxPrintable[A](implicit pa: Printable[A]): Printable[Box[A]] =
      pa.contramap[Box[A]](_.value)

    println(format(Box(15)))
    println(format(Box("hello world")))
    println(format(Box(true)))
  }

  println("---")
}
