package hutil

import java.lang.System.{currentTimeMillis => currentTime}
import scala.collection.mutable.ListBuffer

import hutil.classname._
import hutil.stringformat._

@com.github.ghik.silencer.silent("deprecated")
trait App extends DelayedInit {

  final val executionStart: Long = currentTime

  final protected def args: Array[String] = _args

  private[this] var _args: Array[String] = _

  private[this] val initCode = new ListBuffer[() => Unit]

  def execBody(body: => Unit): Unit =
    try {
      runtimeInfo.blue.boxed.println
      objectName(this).blue.boxed.println
      body
    } finally {
      val total = currentTime - executionStart
      s"[total: $total ms]".blue.boxed.println
    }

  override def delayedInit(body: => Unit): Unit = {
    initCode += (() => execBody(body))
  }

  final def main(args: Array[String]) = {
    this._args = args
    for (proc <- initCode) proc()
  }
}
