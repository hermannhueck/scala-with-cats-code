package sandbox._01_intro._11_typeclass.json

object Main extends App {

  println()

  import JsonWriterInstances._

  val jsStr      = Json.toJson("a String")
  val jsJohn     = Json.toJson(Person("John", "john@example.com"))(personWriter)
  val jsDave     = Json.toJson(Person("Dave", "dave@example.com"))
  val jsSomeStr  = Json.toJson(Some("some String"))(someWriter)
  val jsSomeStr2 = Json.toJson(Some("some String"))
  val jsNoString = Json.toJson(Option.empty[String]) // None changed for migration to 2.13
  val jsSomeDave = Json.toJson(Some(Person("Dave", "dave@example.com")))
  val jsNoPerson = Json.toJson(Option.empty[Person]) // None changed for migration to 2.13

  Json.toJson(Option("A string"))

  showJson(jsStr)
  showJson(jsJohn)
  showJson(jsDave)
  showJson(jsSomeStr)
  showJson(jsSomeStr2)
  showJson(jsNoString)
  showJson(jsSomeDave)
  showJson(jsNoPerson)

  println()

  import JsonSyntax._

  val jsJohn2     = Person("John", "john@example.com").toJson(personWriter)
  val jsDave2     = Person("Dave", "dave@example.com").toJson
  val jsStr2      = "another String".toJson(stringWriter)
  val jsStr3      = "yet another String".toJson
  val jsSomeStr3  = Some("yet another String").toJson
  val jsSomeStr4  = None.asInstanceOf[Option[String]].toJson
  val jsSomeDave2 = Some(Person("Dave", "dave@example.com")).toJson
  val jsNoPerson2 = None.asInstanceOf[Option[Person]].toJson // awful casts -- how to get rid of it ???

  showJson(jsJohn2)
  showJson(jsDave2)
  showJson(jsStr2)
  showJson(jsStr3)
  showJson(jsSomeStr3)
  showJson(jsSomeStr4)
  showJson(jsSomeDave2)
  showJson(jsNoPerson2)

  private def showJson(json: Json): Unit = {
    print(json)
    println("   --   " + json.asString)
  }

  println()
}
