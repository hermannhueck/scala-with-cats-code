package myex.nonemptytraverse

import hutil.syntax.pipe._
import cats.implicits._
import cats.data.NonEmptyList

object Example extends hutil.App {

  val snippets = NonEmptyList.of("What do you do", "What are you doing") |- println
  // snippets: cats.data.NonEmptyList[String] = NonEmptyList(What do you do, What are you doing)

  def countWords(text: String): Map[String, Int] =
    text.split(" ").groupBy(identity).view.mapValues(_.length).toMap

  snippets.nonEmptyTraverse(countWords) | println
  // res0: Map[String,cats.data.NonEmptyList[Int]] = Map(you -> NonEmptyList(1, 1), What -> NonEmptyList(1, 1))

  // Note that, just like traverse, nonEmptyTraverse(f) is equivalent to map(f).nonEmptySequence, so the above could be rewritten as:

  snippets.map(countWords).nonEmptySequence | println
  // res1: Map[String,cats.data.NonEmptyList[Int]] = Map(you -> NonEmptyList(1, 1), What -> NonEmptyList(1, 1))

  snippets.map(countWords) | println
  // res2: cats.data.NonEmptyList[Map[String,Int]] = NonEmptyList(Map(do -> 2, What -> 1, you -> 1), Map(doing -> 1, are -> 1, What -> 1, you -> 1))

  snippets.map(countWords).nonEmptySequence.view.mapValues(_.length).toMap | println
  // res3: Map[String,Int] = Map(you -> 2, What -> 2)
}
