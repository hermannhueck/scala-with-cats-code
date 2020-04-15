package exkittens

import hutil.syntax.pipe._

import cats._
import implicits._
import lift._

object Ex04Lift extends hutil.App {

  def foo(x: Int, y: String, z: Float): String =
    s"$x - $y - $z"

  val lifted = Applicative[Option].liftA(foo _)
  // lifted: (Option[Int], Option[String], Option[Float]) => Option[String] = <function3>

  lifted(Some(1), Some("a"), Some(3.2f)) | println
  // res0: Option[String] = Some(1 - a - 3.2)
}
