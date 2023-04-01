import cats.Monad
import cats.syntax.flatMap._ // For flatMap

def retry[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
  f(start)
    .flatMap { a => retry(a)(f) }

import cats.instances.option._

retry(100) { a =>
  if (a == 0) None else Some(a - 1)
}

// retry(10000) { a =>
//   if (a == 0) None else Some(a - 1)
// } // StackOverflowError

import cats.syntax.functor._ // for map

def retryTailRecM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
  Monad[F].tailRecM(start) { a =>
    f(a).map(a2 => Left(a2))
  }

retryTailRecM(10000) { a =>
  if (a == 0) None else Some(a - 1)
} // no sStackOverflowError

import cats.syntax.monad._ // for iterateWhileM

def retryM[F[_]: Monad, A](start: A)(f: A => F[A]): F[A] =
  start.iterateWhileM(f)(a => true)

retryM(100000) { a =>
  if (a == 0) None else Some(a - 1)
} // no sStackOverflowError
