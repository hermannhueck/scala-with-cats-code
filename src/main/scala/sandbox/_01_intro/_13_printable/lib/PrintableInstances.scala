package sandbox._01_intro._13_printable.lib

import java.util.Date

object PrintableInstances {

  implicit val stringPrintable: Printable[String] = new Printable[String] {
    override def format(value: String): String = value
  }

  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = value.toString
  }

  implicit val datePrintable: Printable[Date] = new Printable[Date] {
    override def format(value: Date): String = value.toString
  }
}
