package scalaz

import Isomorphism._

trait TypeClassIso0[F[_]] {
  def iso[A, B](f: A <=> B)(implicit F: F[B]): F[A]
}

object TypeClassIso0 {
  @inline implicit def apply[F[_]](implicit F: TypeClassIso0[F]): TypeClassIso0[F] = F

  implicit val semigroup: TypeClassIso0[Semigroup] =
    new TypeClassIso0[Semigroup] {
      def iso[A, B](f: A <=> B)(implicit F: Semigroup[B]) =
        new IsomorphismSemigroup[A, B] {
          override def G = F
          override def iso = f
        }
    }

  // TODO more instances
}
