package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Nondeterminism` */
final class NondeterminismOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: Nondeterminism[F]) extends Ops[F[A]] {
  ////

  ////
}

trait ToNondeterminismOps0 {
  implicit final def ToNondeterminismOpsUnapply[FA](v: FA)(implicit F0: Unapply[Nondeterminism, FA]) =
    new NondeterminismOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToNondeterminismOps extends ToNondeterminismOps0 with ToMonadOps {
  implicit final def ToNondeterminismOps[F[_],A](v: F[A])(implicit F0: Nondeterminism[F]) =
    new NondeterminismOps[F,A](v)

  ////

  ////
}

trait NondeterminismSyntax[F[_]] extends MonadSyntax[F] {
  implicit final def ToNondeterminismOps[A](v: F[A]): NondeterminismOps[F, A] = new NondeterminismOps[F,A](v)(NondeterminismSyntax.this.F)

  def F: Nondeterminism[F]
  ////

  ////
}
