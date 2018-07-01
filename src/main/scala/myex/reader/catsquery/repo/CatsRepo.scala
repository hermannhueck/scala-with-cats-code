package myex.reader.catsquery.repo

import myex.reader.catsquery.model._

trait CatsRepo {
  def findById(id: CatId): Option[Cat]
  def findByPredicate(p: Cat => Boolean): List[Cat]
  def findByName(name: String): List[Cat]
  def findAll: List[Cat]
  def countAll: Int
}

object CatsRepo {

  def getRepository(cats: Map[CatId, Cat]): CatsRepo = {
    new CatsRepoImpl(cats)
  }

  private class CatsRepoImpl(cats: Map[CatId, Cat]) extends CatsRepo {
    override def findById(id: CatId): Option[Cat] =
      cats.get(id)
    override def findByPredicate(p: Cat => Boolean): List[Cat] =
      findAll.filter(p(_))
    override def findByName(name: String): List[Cat] =
      findByPredicate(_.name.toLowerCase == name.toLowerCase)
    override def findAll: List[Cat] =
      cats.values.toList
    override def countAll: Int =
      cats.size
  }
}

object CatsRepoFunctions {
  def findById(id: CatId): CatsRepo => Option[Cat] =
    _.findById(id)
  def findByPredicate(p: Cat => Boolean): CatsRepo => List[Cat] =
    _.findByPredicate(p)
  def findByName(name: String): CatsRepo => List[Cat] =
    _.findByName(name)
  def findAll: CatsRepo => List[Cat] =
    _.findAll
  def countAll: CatsRepo => Int =
    _.countAll
}

