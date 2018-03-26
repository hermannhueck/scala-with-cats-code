package sandbox._04_monads._49_state

import cats.data.State

object StateMonad extends App {

  println("--- 4.9.1 Creating and Unpacking State")

  {
    val a = State[Int, String] { state =>
      (state, s"The state is $state")
    }

    val (state, result) = a.run(10).value
    println(state)
    println(result)

    val state2 = a.runS(20).value
    println(state2)

    val result2 = a.runA(20).value
    println(result2)
  }

  println("--- 4.9.2 Composing and Transforming State")

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

  println("---")

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

    val inspectDemo = State.inspect[Int, String](_ + "!")
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

  println("--- 4.9.3 Exercise: Post-Order Calculator")

  {
    type CalcState[A] = State[List[Int], A]

    def evalOne(sym: String): CalcState[Int] = sym match {
      case "+" => operator(_ + _)
      case "-" => operator(_ - _)
      case "*" => operator(_ * _)
      case "/" => operator(_ / _)
      case num => operand(num.toInt)
    }

    def operand(num: Int): CalcState[Int] =
      State[List[Int], Int] { stack =>
        (num :: stack, num)
      }

    def operator(func: (Int, Int) => Int): CalcState[Int] =
      State[List[Int], Int] {
        case a :: b :: tail =>
          val ans = func(a, b)
          (ans :: tail, ans)
        case _ =>
          sys.error("Fail!")
      }

    println(evalOne("42").runA(Nil).value)

    val program = for {
      _   <- evalOne("1")
      _   <- evalOne("2")
      ans <- evalOne("+")
    } yield ans
    println(program.runA(Nil).value) // --> 3

    import cats.syntax.applicative._ // for pure

    def evalAll(input: List[String]): CalcState[Int] =
      input.foldLeft(0.pure[CalcState]) { (a, b) =>
        a.flatMap(_ => evalOne(b))
      }

    val program2 = evalAll(List("1", "2", "+", "3", "*"))
    println(program2.runA(Nil).value) // --> 9

    val program3 = for {
      _   <- evalAll(List("1", "2", "+"))
      _   <- evalAll(List("3", "4", "+"))
      ans <- evalOne("*")
    } yield ans
    println(program3.runA(Nil).value) // --> 21

    def evalInput(input: String): Int =
      evalAll(input.split(" ").toList).runA(Nil).value

    println(evalInput("1 2 + 3 4 + *")) // --> 21
  }

  println("---")
}
