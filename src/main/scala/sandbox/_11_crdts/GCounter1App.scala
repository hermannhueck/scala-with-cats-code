package sandbox._11_crdts

object GCounter1App extends App {

  final case class GCounter(counters: Map[String, Int]) {

    def increment(machine: String, amount: Int): GCounter = {
      val value = amount + counters.getOrElse(machine, 0)
      GCounter(counters + (machine -> value))
    }

    def merge(that: GCounter): GCounter = GCounter {
      that.counters ++ this.counters.map {
        case (machine, thisValue) =>
          val thatValue = that.counters.getOrElse(machine, 0)
          machine -> (thisValue max thatValue)
      }
    }

    def total: Int =
      counters.values.sum
  }

  println("\n-----")

  val g1 = Map("a" -> 7, "b" -> 3)
  val g2 = Map("a" -> 2, "b" -> 5)

  val merged = GCounter(g1) merge GCounter(g2)
  // merged.counters: Map[String,Int] = Map(a -> 7, b -> 5)
  println(merged.counters)

  val total = merged.total
  // total: Int = 12
  println(total)

  println("-----\n")
}
