package sandbox._01_intro._13_printable.lib

trait Printable[A] {

  def format(value: A): String
}

object Printable {

  implicit class PrintableOps[A](value: A) {

    def format(implicit printable: Printable[A]): String = printable.format(value)

    def print(implicit printable: Printable[A]) = println(format)
  }

  def format[A](value: A)(implicit printable: Printable[A]): String = value.format

  def print[A](value: A)(implicit printable: Printable[A]): Unit = value.print
}
