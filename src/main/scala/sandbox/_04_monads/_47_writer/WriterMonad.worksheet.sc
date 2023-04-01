import cats.Id
import cats.data.{Writer, WriterT}
import cats.instances.vector._   // for Monoid
import cats.syntax.applicative._ // for pure
import cats.syntax.writer._      // tell, writer

println("--- 4.7.1 Creating and Unpacking Writers")

// WriterT[Id, Vector[String], Int] == Writer[Vector[String], Int]
val w: WriterT[Id, Vector[String], Int] = Writer(
  Vector(
    "It was the best of times",
    "it was the worst of times"
  ),
  1859
)
println(w)

type Logged[A] = Writer[Vector[String], A]

val l: Logged[Int] = 123.pure[Logged]
println(l)

val l2: Writer[Vector[String], Unit] = Vector("msg1", "msg2", "msg3").tell
println(l2)

val a: WriterT[Id, Vector[String], Int] = Writer(Vector("msg1", "msg2", "msg3"), 123)

val b: Writer[Vector[String], Int] = 123.writer(Vector("msg1", "msg2", "msg3"))

val aResult: Int = a.value
// aResult: Int = 123
println(aResult)

val aLog: Vector[String] = a.written
// aLog: Vector[String] = Vector(msg1, msg2, msg3)
println(aLog)

val (log, result) = b.run
// log: scala.collection.immutable.Vector[String] = Vector(msg1, msg2, msg3)
// result: Int = 123

"> a: "
a
a.value
a.written
b.run

"> b: "
b
b.value
b.written
b.run

println("--- 4.7.2 Composing and Transforming Writers")

val writer1: WriterT[Id, Vector[String], Int] = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

val ran: (Vector[String], Int) = writer1.run

val writer2: WriterT[Id, Vector[String], Int] =
  writer1.mapWritten(_.map(_.toUpperCase))
val ran2: (Vector[String], Int) =
  writer2.run

val writer3: WriterT[Id, Vector[String], Int] = writer1.bimap(
  log => log.map(_.toUpperCase),
  res => res * 100
)
val ran3: (Vector[String], Int) =
  writer3.run

val writer4: WriterT[Id, Vector[String], Int] = writer1.mapBoth { (log, res) =>
  val log2 = log.map(_ + "!")
  val res2 = res * 1000
  (log2, res2)
}
val ran4: (Vector[String], Int) =
  writer4.run

val writer5: WriterT[Id, Vector[String], Int] =
  writer1.reset
val ran5: (Vector[String], Int) =
  writer5.run

val writer6: WriterT[Id, Int, Vector[String]] =
  writer1.swap
val ran6: (Int, Vector[String]) =
  writer6.run
