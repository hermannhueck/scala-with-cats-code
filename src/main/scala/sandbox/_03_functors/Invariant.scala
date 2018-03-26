package sandbox._03_functors

import scala.language.higherKinds

trait Invariant[F[_]] {
  def imap[A, B](fa: F[A])(f: A => B)(g: B => A): F[B]
}
