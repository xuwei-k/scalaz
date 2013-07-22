package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
final class CojoinOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: Cojoin[F]) extends Ops[F[A]] {
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  final def coflatten: F[F[A]] = F.cojoin(self)
  ////
}

trait ToCojoinOps0 {
  implicit final def ToCojoinOpsUnapply[FA](v: FA)(implicit F0: Unapply[Cojoin, FA]) =
    new CojoinOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCojoinOps extends ToCojoinOps0 with ToFunctorOps {
  implicit final def ToCojoinOps[F[_],A](v: F[A])(implicit F0: Cojoin[F]) =
    new CojoinOps[F,A](v)

  ////

  ////
}

trait CojoinSyntax[F[_]] extends FunctorSyntax[F] {
  implicit final def ToCojoinOps[A](v: F[A]): CojoinOps[F, A] = new CojoinOps[F,A](v)(CojoinSyntax.this.F)

  def F: Cojoin[F]
  ////

  ////
}
