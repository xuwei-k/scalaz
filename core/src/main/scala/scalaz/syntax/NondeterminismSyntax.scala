package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Nondeterminism` */
final class NondeterminismOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Nondeterminism[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToNondeterminismOps0 {


}

trait ToNondeterminismOps extends ToNondeterminismOps0 with ToMonadOps {
  implicit def ToNondeterminismOps[F[_],A](v: F[A])(implicit F0: Nondeterminism[F]) =
    new NondeterminismOps[F,A](v)

  ////

  ////
}

trait NondeterminismSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToNondeterminismOps[A](v: F[A]): NondeterminismOps[F, A] = new NondeterminismOps[F,A](v)(NondeterminismSyntax.this.F)

  def F: Nondeterminism[F]
  ////

  ////
}
