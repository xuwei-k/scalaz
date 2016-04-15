package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Comonad` */
final class ComonadOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Comonad[F]) extends Ops[F[A]] {
  ////
  def copoint: A = F.copoint(self)

  ////
}

sealed trait ToComonadOps0 {


}

trait ToComonadOps extends ToComonadOps0 with ToCobindOps {
  implicit def ToComonadOps[F[_],A](v: F[A])(implicit F0: Comonad[F]) =
    new ComonadOps[F,A](v)

  ////

  ////
}

trait ComonadSyntax[F[_]] extends CobindSyntax[F] {
  implicit def ToComonadOps[A](v: F[A]): ComonadOps[F, A] = new ComonadOps[F,A](v)(ComonadSyntax.this.F)

  def F: Comonad[F]
  ////

  ////
}
