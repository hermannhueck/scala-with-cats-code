package sandbox._04_monads._46_eval

import cats.Eval

object EvalMonad extends App {

  println("--- 4.6.2 Eval’s Models of Evaluation")

  {
    val now: Eval[Double] = Eval.now(math.random + 1000)
    // now: cats.Eval[Double] = Now(1000.6884369117727)

    val later: Eval[Double] = Eval.later(math.random + 2000)
    // later: cats.Eval[Double] = cats.Later@71175ee9

    val always: Eval[Double] = Eval.always(math.random + 3000)
    // always: cats.Eval[Double] = cats.Always@462e2fea

    println(now.value)
    // res6: Double = 1000.6884369117727

    println(later.value)
    // res7: Double = 2000.8775276106762

    println(always.value)
    // res8: Double = 3000.6943184468
  }

  println("--- 4.6.3 Eval as a Monad")

  {
    println("--- greeting ---")
    val greeting = Eval.always {
      println("Step 1")
      "Hello"
    }.map { str =>
      println("Step 2")
      s"$str world"
    }
    println(greeting.value)

    println("--- ans ---")
    val ans = for {
      a <- Eval.now {
        println("Calculating A"); 40
      }
      b <- Eval.always {
        println("Calculating B"); 2
      }
    } yield {
      println("Adding A and B")
      a + b
    }
    println(ans.value)
    println(ans.value)

    println("--- saying ---")
    val saying = Eval.
      always {
        println("Step 1"); "The cat"
      }.
      map { str => println("Step 2"); s"$str sat on" }.
      memoize.
      map { str => println("Step 3"); s"$str the mat" }
    println(saying.value)
    println(saying.value)
  }

  println("--- 4.6.4 Trampolining and Eval.defer")

  {
    def factorial(n: BigInt): BigInt =
      if (n == 1) n else n * factorial(n - 1)

    // factorial(50000) // java.lang.StackOverflowError

    def factorial2(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        factorial2(n - 1).map(_ * n)
      }

    // factorial2(50000).value // java.lang.StackOverflowError

    def factorial3(n: BigInt): Eval[BigInt] =
      if (n == 1) {
        Eval.now(n)
      } else {
        Eval.defer(factorial3(n - 1).map(_ * n))
      }

    println(factorial3(50000).value.toString().take(100) + "...")
  }

  println("--- 4.6.5 Exercise: Safer Folding using Eval")

  {
    // not stack safe
    def foldRightNotStackSafe[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      as match {
        case head :: tail => fn(head, foldRightNotStackSafe(tail, acc)(fn))
        case Nil => acc
      }

    // stack safe
    def foldRightEval[A, B](as: List[A], acc: Eval[B])(fn: (A, Eval[B]) => Eval[B]): Eval[B] =
      as match {
        case head :: tail => Eval.defer(fn(head, foldRightEval(tail, acc)(fn)))
        case Nil => acc
      }

    def foldRight[A, B](as: List[A], acc: B)(fn: (A, B) => B): B =
      foldRightEval(as, Eval.now(acc)) { (a, b) =>
        b.map(fn(a, _))
      }.value

    val result: Long = foldRight((1 to 100000).toList, 0L)(_ + _)
    println(result)
  }

  println("---")
}
