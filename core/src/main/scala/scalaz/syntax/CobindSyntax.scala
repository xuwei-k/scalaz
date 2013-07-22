package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cobind` */
final class CobindOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: Cobind[F]) extends Ops[F[A]] {
  ////
  def cobind[B](f: F[A] => B) = F.cobind(self)(f)
  def coflatMap[B](f: F[A] => B) = F.cobind(self)(f)
  ////
}

trait ToCobindOps0 {
  implicit final def ToCobindOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cobind, FA]) =
    new CobindOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCobindOps extends ToCobindOps0 with ToFunctorOps {
  implicit final def ToCobindOps[F[_],A](v: F[A])(implicit F0: Cobind[F]) =
    new CobindOps[F,A](v)

  ////

  ////
}

trait CobindSyntax[F[_]] extends FunctorSyntax[F] {
  implicit final def ToCobindOps[A](v: F[A]): CobindOps[F, A] = new CobindOps[F,A](v)(CobindSyntax.this.F)

  def F: Cobind[F]
  ////

  ////
}
