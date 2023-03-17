package sandbox._01_intro._13_printable

import java.util.Date

import sandbox._01_intro._13_printable.domain.Cat
import sandbox._01_intro._13_printable.lib.Printable
import sandbox._01_intro._13_printable.lib.PrintableInstances._
import sandbox._01_intro._13_printable.lib.Printable._

object Main extends App {

  // overrides PrintableInstances.intPrintable which is in scope but has lower priority
  implicit val intPrintable: Printable[Int] = new Printable[Int] {
    override def format(value: Int): String = "How many cats? " + value.toString
  }

  // overrides PrintableInstances.datePrintable which is in scope but has lower priority
  implicit val datePrintable: Printable[Date] = new Printable[Date] {
    override def format(value: Date): String = "Date of meeting: " + value.toString
  }

  println()

  val mizzi    = Cat("Mizzi", 1, "black")
  val garfield = Cat("Garfield", 38, "ginger and black")

  Printable.print("Cats are meeting here!")
  Printable.print(2)
  Printable.print(mizzi)
  Printable.print(garfield)
  Printable.print(new Date)

  println("\n--> now using extension methods (type enrichment) ...\n")

  "Cats are meeting here!".print
  2.print
  mizzi.print
  garfield.print
  new Date().print

  println()
}
