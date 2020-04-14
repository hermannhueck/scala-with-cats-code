package myex.validated.config

import cats.data.Validated
import cats.data.Validated.{Invalid, Valid}

case class Config(map: Map[String, String]) {
  def parse[A: Read](key: String): Validated[ConfigError, A] =
    map.get(key) match {
      case None => Invalid(MissingConfig(key))
      case Some(value) =>
        Read[A].read(value) match {
          case None    => Invalid(ParseError(key))
          case Some(a) => Valid(a)
        }
    }
}
