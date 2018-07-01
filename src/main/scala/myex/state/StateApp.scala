package myex.state

import cats.implicits._

object StateApp extends App {

  println("\n----- Processing state\n")

  {
    println("\n----- processState 1")

    def add(state: List[Int], value: Int): (List[Int], Int) = {
      val sum = state.head + value
      (sum :: state, sum)
    }

    def processState: List[Int] => (List[Int], Int) =
      for {
        r1 <- (li: List[Int]) => (li, ())
        _ = println("-- " + r1)
        (s1, i1) = r1
        r2 <- (li: List[Int]) => add(s1, 1)
        _ = println("-- " + r2)
        (s2, i2) = r2
        r3 <- (li: List[Int]) => add(s2, 10)
        _ = println("-- " + r3)
        (s3, i3) = r3
        r4 <- (li: List[Int]) => add(s3, 100)
        _ = println("-- " + r4)
      } yield r4

    val result: (List[Int], Int) = processState(List(0))
    println(result)
  }

  {
    println("\n----- processState 2")

    type IntList = List[Int]
    type IntState = IntList => (IntList, Int)

    def set(state: IntList): (IntList, Unit) =
      (state, ())

    def get(state: IntList): (IntList, Int) =
      (state, state.head)

    def modify(state: IntList, f: IntList => Int): (IntList, Int) = {
      val newValue = f(state)
      (newValue :: state, newValue)
    }

    def processState: IntState =
      for {
        r1 <- (li: IntList) => set(li)
        r2 <- (li: IntList) => modify(r1._1, _.head + 1)
        r3 <- (li: IntList) => modify(r2._1, _.head + 10)
        r4 <- (li: IntList) => modify(r3._1, _.head + 100)
        r5 <- (li: IntList) => get(r4._1)
      } yield r5

    val result = processState(List(0))
    println(result)
  }

  {
    println("\n----- processState 3")

    type IntList = List[Int]
    type IntState = IntList => (IntList, Int)

    def set: IntList => (IntList, Unit) =
      state => (state, ())

    def get: IntList => (IntList, Int) =
      state => (state, state.head)

    def modify(f: IntList => Int): IntList => (IntList, Int) =
      state => {
        val newValue = f(state)
        (newValue :: state, newValue)
      }

    def processState: IntState =
      for {
        r1 <- get
        r2 <- (_: IntList) => modify(_.head + 1)(r1._1)
        r3 <- (_: IntList) => modify(_.head + 10)(r2._1)
        r4 <- (_: IntList) => modify(_.head + 100)(r3._1)
        r5 <- (_:IntList) => get(r4._1)
      } yield r5

    val result = processState(List(0))
    println(result)
  }

  {
    println("\n----- processState 4: using State Monad")

    import cats._, cats.data._

    type IntList = List[Int]
    type StateFunction = IntList => (IntList, Int)
    type StateIntList = State[IntList, Int]

    def add(state: IntList, value: Int): IntList = {
      val sum = state.head + value
      sum :: state
    }

    def processState: StateIntList =
      for {
        r0 <- State.get[IntList]
        _ <- State.set[IntList](r0) // not necessary, just to show State.set
        _ <- State.modify[IntList](li => add(li, 1))
        _ <- State.modify[IntList](li => add(li, 10))
        _ <- State.modify[IntList](li => add(li, 100))
        r1 <- State.inspect[IntList, Int](_.head)
      } yield r1

    val result: (IntList, Int) = processState.run(List(0)).value
    println(result)
  }

  println("-----\n")
}
