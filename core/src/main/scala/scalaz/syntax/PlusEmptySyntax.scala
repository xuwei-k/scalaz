package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `PlusEmpty` */
final class PlusEmptyOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: PlusEmpty[F]) extends Ops[F[A]] {
  ////

  ////
}

trait ToPlusEmptyOps0 {
  implicit final def ToPlusEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[PlusEmpty, FA]) =
    new PlusEmptyOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToPlusEmptyOps extends ToPlusEmptyOps0 with ToPlusOps {
  implicit final def ToPlusEmptyOps[F[_],A](v: F[A])(implicit F0: PlusEmpty[F]) =
    new PlusEmptyOps[F,A](v)

  ////

  ////
}

trait PlusEmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit final def ToPlusEmptyOps[A](v: F[A]): PlusEmptyOps[F, A] = new PlusEmptyOps[F,A](v)(PlusEmptySyntax.this.F)

  def F: PlusEmpty[F]
  ////

  ////
}
