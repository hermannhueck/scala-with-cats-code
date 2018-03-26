package sandbox._03_functors

import scala.language.higherKinds

trait Contravariant[F[_]] {
  def contramap[A, B](fa: F[A])(f: B => A): F[B]
}
