package sandbox._01_intro._11_typeclass.json_improved.lib

// Define a very simple JSON AST
sealed trait Json {
  def asString: String
}

final case class JsObject(get: Map[String, Json]) extends Json {
  override val asString: String =
    get.map {
      case (name, value) => "\"" + name + "\":" + value.asString
    }
      .mkString("{", ",", "}")
}

final case class JsString(get: String) extends Json {
  override val asString: String = "\"" + get.replaceAll("\\|\"", "\\\\$1") + "\""
}

final case class JsNumber(get: Double) extends Json {
  override val asString: String = get.toString
}

case object JsNull extends Json {
  override val asString: String = "null"
}


// The "serialize to JSON" behaviour is encoded in this trait.
// This is the type class, a trait with at least one type parameter.
trait JsonWriter[A] {
  def write(value: A): Json
}

object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)

  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit writer: JsonWriter[A]): Json =
      writer.write(value)
  }

}
