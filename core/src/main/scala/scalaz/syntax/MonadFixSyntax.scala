package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadFix` */
trait MonadFixOps[F[_],A] extends Ops[F[A]] {
  implicit def F: MonadFix[F]
  ////

  ////
}

trait ToMonadFixOps0 {
  implicit def ToMonadFixOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadFix, FA]) =
    new MonadFixOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadFix[F0.M] = F0.TC }

}

trait ToMonadFixOps extends ToMonadFixOps0 with ToMonadOps {
  implicit def ToMonadFixOps[F[_],A](v: F[A])(implicit F0: MonadFix[F]) =
    new MonadFixOps[F,A] { def self = v; implicit def F: MonadFix[F] = F0 }

  ////

  ////
}

trait MonadFixSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToMonadFixOps[A](v: F[A]): MonadFixOps[F, A] = new MonadFixOps[F,A] { def self = v; implicit def F: MonadFix[F] = MonadFixSyntax.this.F }

  def F: MonadFix[F]
  ////

  ////
}
