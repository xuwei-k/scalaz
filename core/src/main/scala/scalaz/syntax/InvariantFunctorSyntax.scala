package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `InvariantFunctor` */
final class InvariantFunctorOps[F[_],A] private[syntax](val self: F[A])(implicit val F: InvariantFunctor[F]) extends Ops[F[A]] {
  ////

  import BijectionT.Bijection
  import Isomorphism.<=>

  final def xmap[B](f: A => B, g: B => A): F[B] = F.xmap(self, f, g)
  final def xmapb[B](b: Bijection[A, B]): F[B] = F.xmapb(self)(b)
  final def xmapi[B](iso: A <=> B): F[B] = F.xmapi(self)(iso)
  ////
}

sealed trait ToInvariantFunctorOps0 {


}

trait ToInvariantFunctorOps extends ToInvariantFunctorOps0 {
  implicit def ToInvariantFunctorOps[F[_],A](v: F[A])(implicit F0: InvariantFunctor[F]) =
    new InvariantFunctorOps[F,A](v)

  ////

  ////
}

trait InvariantFunctorSyntax[F[_]]  {
  implicit def ToInvariantFunctorOps[A](v: F[A]): InvariantFunctorOps[F, A] = new InvariantFunctorOps[F,A](v)(InvariantFunctorSyntax.this.F)

  def F: InvariantFunctor[F]
  ////

  ////
}
