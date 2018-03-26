package sandbox._03_functors._36_contrainvariant

object ImapExercise extends App {

  println("---")


  trait Codec[A] {

    def encode(value: A): String
    def decode(value: String): A

    def imap[B](dec: A => B, enc: B => A): Codec[B] = {
      val self = this
      new Codec[B] {
        override def encode(value: B): String = self.encode(enc(value))
        override def decode(value: String): B = dec(self.decode(value))
      }
    }
  }


  def encode[A](value: A)(implicit c: Codec[A]): String =
    c.encode(value)

  def decode[A](value: String)(implicit c: Codec[A]): A =
    c.decode(value)

  implicit val stringCodec: Codec[String] =
    new Codec[String] {
      def encode(value: String): String = value
      def decode(value: String): String = value
    }

  implicit val intCodec: Codec[Int] =
    stringCodec.imap(_.toInt, _.toString)

  val s: String = encode(5)
  val i: Int = decode(s)(intCodec)
  println(s == "5" && i == 5)

  implicit val booleanCodec: Codec[Boolean] =
    stringCodec.imap(_.toBoolean, _.toString)

  val b = decode(encode(false))(booleanCodec)
  println(b == false)

  implicit val doubleCodec: Codec[Double] =
    stringCodec.imap(_.toDouble, _.toString)

  val d = decode(encode(0.83))(doubleCodec)
  println(d == 0.83)

  final case class Box[A](value: A)

  implicit def boxCodec[A](implicit c: Codec[A]): Codec[Box[A]] =
    c.imap[Box[A]](Box(_), _.value)

  val s2 = encode(Box(0.83))(boxCodec(doubleCodec))
  val box2 = decode(s2)(boxCodec(doubleCodec))
  println(box2 == Box(0.83))

  val s3 = encode(Box(0.92))
  val box3 = decode(s3)(boxCodec[Double])
  println(box3.value == 0.92)

  println(encode(123.4))
  println(decode[Double]("123.4"))
  println(encode(Box(123.4)))
  println(decode[Box[Double]]("123.4"))

  println("---")
}
