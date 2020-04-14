package hutil

package object stringformat { self =>

  val javaVendor           = scala.util.Properties.javaVendor
  val javaVersion          = scala.util.Properties.javaVersion
  val scalaVersion: String = scala.util.Properties.versionNumberString

  val runtimeInfo: String =
    s"JDK: $javaVendor $javaVersion,   scala.version: $scalaVersion"

  def textBoxed(
      text: String,
      width: Int = 100,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String = {
    val frontPad    = fill * 10
    val startLength = (10 + text.length() + 2)
    val endLength   = if (startLength > width) 0 else width - startLength
    val endPad      = fill * endLength
    s"$leading$frontPad $text $endPad$trailing" // tap (s => println(s.length()))
  }

  def dash(
      width: Int = 100,
      leading: String = "",
      trailing: String = "",
      fill: String = "\u2500"
  ): String =
    s"$leading${fill * width}$trailing"

  @inline def dash10: String  = dash(10)
  @inline def dash50: String  = dash(50)
  @inline def dash80: String  = dash(80)
  @inline def dash100: String = dash(100)

  implicit class StringSyntax(private val what: String) extends AnyVal {

    @inline def boxed: String =
      what.textBoxed()

    @inline def textBoxed(width: Int = 100): String =
      self.textBoxed(what, width)

    @inline def colored(escape: String): String =
      s"$escape$what${Console.RESET}"

    @inline def red: String =
      what.colored(Console.RED)

    @inline def green: String =
      what.colored(Console.GREEN)

    @inline def blue: String =
      what.colored(Console.BLUE)

    @inline def yellow: String =
      what.colored(Console.YELLOW)

    @inline def cyan: String =
      what.colored(Console.CYAN)

    @inline def magenta: String =
      what.colored(Console.MAGENTA)

    @inline def black: String =
      what.colored(Console.BLACK)

    @inline def white: String =
      what.colored(Console.WHITE)

    @inline def underlined: String =
      what.colored(Console.UNDERLINED)

    @inline def bold: String =
      what.colored(Console.BOLD)

    @inline def reversed: String =
      what.colored(Console.REVERSED)

    @inline def reset: String =
      what.colored(Console.RESET)

    @inline def print(): Unit =
      Console.print(what)

    @inline def println(): Unit =
      Console.println(what)
  }
}
