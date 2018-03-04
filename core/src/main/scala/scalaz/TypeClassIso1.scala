package scalaz

import Isomorphism._

trait TypeClassIso1[F[_[_]]] {
  def iso[G[_], H[_]](f: G <~> H)(implicit F: F[H]): F[G]
}

object TypeClassIso1 {
  @inline implicit def apply[F[_[_]]](implicit F: TypeClassIso1[F]): TypeClassIso1[F] = F

  implicit val functor: TypeClassIso1[Functor] =
    new TypeClassIso1[Functor] {
      def iso[G[_], H[_]](f: G <~> H)(implicit F: Functor[H]) =
        new IsomorphismFunctor[G, H] {
          override def G = F
          override def iso = f
        }
    }

  implicit val monad: TypeClassIso1[Monad] =
    new TypeClassIso1[Monad] {
      def iso[G[_], H[_]](f: G <~> H)(implicit F: Monad[H]) =
        new IsomorphismMonad[G, H] {
          override def G = F
          override def iso = f
        }
    }

  // TODO more instances
}
