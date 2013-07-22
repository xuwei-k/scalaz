package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `IsEmpty` */
final class IsEmptyOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: IsEmpty[F]) extends Ops[F[A]] {
  ////

  ////
}

trait ToIsEmptyOps0 {
  implicit final def ToIsEmptyOpsUnapply[FA](v: FA)(implicit F0: Unapply[IsEmpty, FA]) =
    new IsEmptyOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToIsEmptyOps extends ToIsEmptyOps0 with ToPlusEmptyOps {
  implicit final def ToIsEmptyOps[F[_],A](v: F[A])(implicit F0: IsEmpty[F]) =
    new IsEmptyOps[F,A](v)

  ////

  ////
}

trait IsEmptySyntax[F[_]] extends PlusEmptySyntax[F] {
  implicit final def ToIsEmptyOps[A](v: F[A]): IsEmptyOps[F, A] = new IsEmptyOps[F,A](v)(IsEmptySyntax.this.F)

  def F: IsEmpty[F]
  ////

  ////
}
