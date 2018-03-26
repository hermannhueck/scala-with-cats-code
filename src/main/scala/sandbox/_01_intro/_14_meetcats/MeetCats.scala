package sandbox._01_intro._14_meetcats

import java.util.Date

import cats.Show
import cats.instances.int._
import cats.instances.string._
import cats.instances.option._


object MeetCats extends App {

  println("\n--> using Show.apply ...\n")

  val showInt:    Show[Int]    = Show.apply[Int]
  val showString: Show[String] = Show[String]

  val intAsString: String = showInt.show(123)
  val stringAsString: String = showString.show("abc")

  println(intAsString)
  println(stringAsString)

  println("\n--> using interface syntax ...\n")

  import cats.syntax.show._ // for show

  val shownInt = 123.show
  val shownString = "abc".show

  println(shownInt)
  println(shownString)
  println(123.show)
  println("abc".show)

  println("\n--> using show for Option ...\n")

  println(Option("abc").show)
  // println(Some("abc").show)   // this doesn't work
  val opt: Option[String] = Some("abc") // this works!
  println(opt.show)
  val opt2: Option[String] = None // this works only if the type of opt2 is specified
  println(opt2.show)
  println(Option.empty[String].show)

  println("\n--> using show for java.util.Date ...\n")

  showDate()
  showDate2()
  showDate3()


  println("\n--> using show for Cat ...\n")

  final case class Cat(name: String, age: Int, color: String)

  implicit val catShow: Show[Cat] = Show.show{ cat =>
    val name  = cat.name.show
    val age   = cat.age.show
    val color = cat.color.show
    s"$name is a $age year-old $color cat."
  }

  val mizzi = Cat("Mizzi", 1, "black and white")
  val garfield = Cat("Garfield", 38, "ginger and black")

  println(mizzi.show)
  println(garfield.show)

  println

  private def showDate(): Unit = {
    implicit val dateShow: Show[Date] =
      new Show[Date] {
        def show(date: Date): String =
          s"${date.getTime} ms since the epoch."
      }
    println(new Date().show)
  }

  private def showDate2(): Unit = {
    implicit val dateShow: Show[Date] = Show.show(date => s"${date.getTime} ms since the epoch.")
    println(new Date().show)
  }

  private def showDate3(): Unit = {
    implicit val dateShow: Show[Date] = Show.fromToString
    println(new Date().show)
  }
}
