package myex.reader.catsquery.model

final case class Cat(id: CatId, name: String, age: Int, kitten: List[CatId])

