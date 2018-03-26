package sandbox._01_intro._11_typeclass.json_improved.domain

import sandbox._01_intro._11_typeclass.json_improved.lib.{JsObject, JsString, Json, JsonWriter}

final case class Person(name: String, email: String)

object Person {

  implicit val personWriter: JsonWriter[Person] =
    new JsonWriter[Person] {
      def write(value: Person): Json =
        JsObject(Map(
          "name" -> JsString(value.name),
          "email" -> JsString(value.email)
        ))
    }
}
