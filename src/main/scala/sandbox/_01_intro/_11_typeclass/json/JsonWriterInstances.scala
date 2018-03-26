package sandbox._01_intro._11_typeclass.json

object JsonWriterInstances {

  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
    }

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }

  // unfortunately works only for Option[A], not for Some[A] or None.type
  implicit def optionWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Option[A]] =
    new JsonWriter[Option[A]] {
      def write(option: Option[A]): Json =
        option match {
          case Some(aValue) => writer.write(aValue)
          case None         => JsNull
        }
    }

  implicit def someWriter[A](implicit optionWriter: JsonWriter[Option[A]]): JsonWriter[Some[A]] =
    optionWriter.asInstanceOf[JsonWriter[Some[A]]] // someWriter is the optionWriter cast to JsonWriter[Some[A]]

  implicit def noneWriter[A](implicit optionWriter: JsonWriter[Option[A]]): JsonWriter[None.type] =
    optionWriter.asInstanceOf[JsonWriter[None.type]] // noneWriter is the optionWriter cast to JsonWriter[None.type]

  // etc...
}