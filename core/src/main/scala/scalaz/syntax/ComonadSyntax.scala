package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
final class ComonadOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: Comonad[F]) extends Ops[F[A]] {
  ////
  def copoint: A = F.copoint(self)

  ////
}

trait ToComonadOps0 {
  implicit final def ToComonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[Comonad, FA]) =
    new ComonadOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToComonadOps extends ToComonadOps0 with ToCojoinOps with ToCobindOps {
  implicit final def ToComonadOps[F[_],A](v: F[A])(implicit F0: Comonad[F]) =
    new ComonadOps[F,A](v)

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CojoinSyntax[F] with CobindSyntax[F] {
  implicit final def ToComonadOps[A](v: F[A]): ComonadOps[F, A] = new ComonadOps[F,A](v)(ComonadSyntax.this.F)

  def F: Comonad[F]
  ////

  ////
}
