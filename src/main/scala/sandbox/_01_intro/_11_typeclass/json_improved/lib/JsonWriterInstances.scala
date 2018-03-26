package sandbox._01_intro._11_typeclass.json_improved.lib

object JsonWriterInstances {

  implicit val stringWriter: JsonWriter[String] =
    new JsonWriter[String] {
      def write(value: String): Json =
        JsString(value)
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

  /*
    implicit def someWriter[A](implicit writer: JsonWriter[A]): JsonWriter[Some[A]] =
      optionWriter[A](writer).asInstanceOf[JsonWriter[Some[A]]]

    implicit def noneWriter[A](implicit writer: JsonWriter[A]): JsonWriter[None.type] =
      // optionWriter[Nothing](writer).asInstanceOf[JsonWriter[None.type]]
      new JsonWriter[None.type] {
        def write(option: None.type): Json = JsNull
      }
  */


  /*
    implicit def optionWriter[A, B <: A](implicit writer: JsonWriter[A], ev: Option[B] <:< Option[A]): JsonWriter[Option[B]] =
      new JsonWriter[Option[B]] {
        def write(option: Option[B]): Json =
          option match {
            case Some(aValue) => writer.write(aValue)
            case None         => JsNull
          }
      }
  */

  // etc...
}