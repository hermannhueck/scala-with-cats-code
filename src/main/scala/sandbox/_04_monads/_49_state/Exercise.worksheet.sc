import cats.data.State

// --- 4.9.3 Exercise: Post-Order Calculator"

type CalcState[A] = State[List[Int], A]

def evalOne(sym: String): CalcState[Int] = sym match {
  case "+" => operator(_ + _)
  case "-" => operator(_ - _)
  case "*" => operator(_ * _)
  case "/" => operator(_ / _)
  case num => operand(num.toInt)
}

def operand(num: Int): CalcState[Int] =
  State[List[Int], Int] { stack => (num :: stack, num) }

def operator(func: (Int, Int) => Int): CalcState[Int] =
  State[List[Int], Int] {
    case a :: b :: tail =>
      val ans = func(b, a) // swap order
      (ans :: tail, ans)
    case _ =>
      sys.error("Fail!")
  }

evalOne("42").runA(Nil).value

val program = for {
  _   <- evalOne("1")
  _   <- evalOne("2")
  ans <- evalOne("+")
} yield ans
program.runA(Nil).value

import cats.syntax.applicative._ // for pure

def evalAll(input: List[String]): CalcState[Int] =
  input.foldLeft(0.pure[CalcState]) { (state, sym) => state.flatMap(_ => evalOne(sym)) }

val program2 = evalAll(List("1", "2", "+", "3", "*"))
program2.runA(Nil).value

evalAll(List("1", "2", "-", "3", "*")).runA(Nil).value

val program3 = for {
  _   <- evalAll(List("1", "2", "+"))
  _   <- evalAll(List("3", "4", "+"))
  ans <- evalOne("*")
} yield ans
program3.runA(Nil).value

def evalInput(input: String): Int = {
  val symbols = input.split(" ").toList
  evalAll(symbols).runA(Nil).value
}

evalInput("1 2 + 3 4 + * 2 *")
