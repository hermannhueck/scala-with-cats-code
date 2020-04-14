package myex.validated.config

trait Read[A] {
  def read(s: String): Option[A]
}

object Read {

  def apply[A](implicit read: Read[A]): Read[A] = read

  implicit val stringRead: Read[String] =
    new Read[String] {
      def read(s: String): Option[String] = Some(s)
    }

  implicit val intRead: Read[Int] =
    new Read[Int] {
      def read(s: String): Option[Int] =
        if (s.matches("-?[0-9]+")) Some(s.toInt)
        else None
    }
}
