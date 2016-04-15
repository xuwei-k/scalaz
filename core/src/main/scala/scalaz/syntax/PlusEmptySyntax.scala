package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `PlusEmpty` */
final class PlusEmptyOps[F[_],A] private[syntax](val self: F[A])(implicit val F: PlusEmpty[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToPlusEmptyOps0 {


}

trait ToPlusEmptyOps extends ToPlusEmptyOps0 with ToPlusOps {
  implicit def ToPlusEmptyOps[F[_],A](v: F[A])(implicit F0: PlusEmpty[F]) =
    new PlusEmptyOps[F,A](v)

  ////

  def mempty[F[_], A](implicit F: PlusEmpty[F]): F[A] = F.empty[A]
  ////
}

trait PlusEmptySyntax[F[_]] extends PlusSyntax[F] {
  implicit def ToPlusEmptyOps[A](v: F[A]): PlusEmptyOps[F, A] = new PlusEmptyOps[F,A](v)(PlusEmptySyntax.this.F)

  def F: PlusEmpty[F]
  ////

  ////
}
