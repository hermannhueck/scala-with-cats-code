package myex.reader.catsquery.app

// import cats._, cats.data._, cats.implicits._ // everything from cats
import cats.instances.function._
import cats.syntax.functor._
import cats.syntax.flatMap._

import myex.reader.catsquery.model._
import myex.reader.catsquery.repo._

object CatsQueryApp extends App {

  val myCats: Map[CatId, Cat] = Map[CatId, Cat](
    1 -> Cat(1, "mizzi", 12, List(2, 3, 4)),
    2 -> Cat(2, "Mimi", 1, List()),
    3 -> Cat(3, "Milli", 1, List()),
    4 -> Cat(4, "Mauzi", 1, List()),
    5 -> Cat(5, "Carlo Tomcat", 18, List()),
    6 -> Cat(6, "Minka", 8, List(7, 8)),
    7 -> Cat(7, "Freddy", 0, List()),
    8 -> Cat(8, "Sammy", 0, List())
  )
  val repo = CatsRepo.getRepository(myCats)

  def findMotherWithKitten(name: String): CatsRepo => (Cat, List[Cat]) = {
    import myex.reader.catsquery.repo.CatsRepoFunctions._
    for { // RepoFunction context
      found  <- findByName(name)
      mother = found.head
      kitten <- findByPredicate(c => mother.kitten.contains(c.id))
    } yield (mother, kitten)
  }

  def findMothersWithKitten: CatsRepo => List[(Cat, List[Cat])] = {

    import myex.reader.catsquery.repo.CatsRepoFunctions._

    def findMothersKitten(mothers: List[Cat]): CatsRepo => List[(Cat, List[Cat])] =
      (repo: CatsRepo) =>
        for { // List context
          mother <- mothers
          kitten = mother.kitten.map(id => findById(id)(repo).get)
        } yield (mother, kitten)

    for { // RepoFunction context
      mothers           <- findByPredicate(_.kitten.nonEmpty)
      mothersWithKitten <- findMothersKitten(mothers)
    } yield mothersWithKitten
  }

  def findCatsWithoutKitten: CatsRepo => List[Cat] =
    CatsRepoFunctions.findByPredicate(_.kitten.isEmpty)

  println()

  println("\n----- Mizzi with her kitten")
  val findMizziWithKitten: CatsRepo => (Cat, List[Cat]) = findMotherWithKitten("mizzi")
  val (mother, kitten)                                  = findMizziWithKitten(repo)
  println(s"mother: $mother")
  println(s"  kitten: ${kitten.mkString(", ")}")

  println("\n----- All cat mamas with her kitten")
  val mothersWithKitten: List[(Cat, List[Cat])] = findMothersWithKitten(repo)
  mothersWithKitten foreach {
    case (mama, kitties) =>
      println(s"mother: $mama")
      println(s"  kitten: ${kitties.mkString(", ")}")
  }

  println("\n----- Cats without kitten")
  val catsWithoutKitten: List[Cat] = findCatsWithoutKitten(repo)
  catsWithoutKitten.sortWith(_.id < _.id) foreach { cat => println(s"$cat") }

  println("\n-----\n")
}
