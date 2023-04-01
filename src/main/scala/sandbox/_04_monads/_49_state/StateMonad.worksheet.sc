import cats.data.State

// --- 4.9.1 Creating and Unpacking State

{
  val a = State[Int, String] { state => (state, s"The state is $state") }

  val (state, result) = a.run(10).value
  println(state)
  println(result)

  val state2 = a.runS(20).value
  println(state2)

  val result2 = a.runA(20).value
  println(result2)
}

// --- 4.9.2 Composing and Transforming State

{
  val step1 = State[Int, String] { num =>
    val ans = num + 1
    (ans, s"Result of step1: $ans")
  }

  val step2 = State[Int, String] { num =>
    val ans = num * 2
    (ans, s"Result of step2: $ans")
  }

  val both = for {
    a <- step1
    b <- step2
  } yield (a, b)

  val (state, result) = both.run(20).value
  println(state)
  println(result)
}

// ---

{
  /*
      The general model for using the State monad is to represent each step of a computation as an instance and compose
      the steps using the standard monad operators. Cats provides several convenience constructors for creating primitive steps:

      - get: extracts the state as the result;
      - set: updates the state and returns unit as the result;
      - pure: ignores the state and returns a supplied result;
      - inspect: extracts the state via a transformation function;
      - modify: updates the state using an update function.
   */
  val getDemo = State.get[Int]
  println(getDemo.run(10).value)

  val setDemo = State.set[Int](30)
  println(setDemo.run(10).value)

  val pureDemo = State.pure[Int, String]("Result")
  println(pureDemo.run(10).value)

  val inspectDemo = State.inspect[Int, String](str => s"$str!")
  println(inspectDemo.run(10).value)

  val modifyDemo = State.modify[Int](_ + 1)
  println(modifyDemo.run(10).value)

  println("---")

  import State._

  val program: State[Int, (Int, Int, Int)] = for {
    a <- get[Int]
    _ <- set[Int](a + 1)
    b <- get[Int]
    _ <- modify[Int](_ + 1)
    c <- inspect[Int, Int](_ * 1000)
  } yield (a, b, c)

  val (state, result) = program.run(1).value
  println(state)
  println(result)
}
