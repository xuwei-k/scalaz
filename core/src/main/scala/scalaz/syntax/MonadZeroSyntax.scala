package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadZero` */
trait MonadZeroOps[F[_],A] extends Ops[F[A]] {
  implicit def F: MonadZero[F]
  ////

  def filter(f: A => Boolean) =
    F.filter(self)(f)

  def withFilter(f: A => Boolean) =
    filter(f)
  ////
}

trait ToMonadZeroOps0 {
  implicit def ToMonadZeroOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadZero, FA]) =
    new MonadZeroOps[F0.M,F0.A] { def self = F0(v); implicit def F: MonadZero[F0.M] = F0.TC }

}

trait ToMonadZeroOps extends ToMonadZeroOps0 with ToMonadOps {
  implicit def ToMonadZeroOps[F[_],A](v: F[A])(implicit F0: MonadZero[F]) =
    new MonadZeroOps[F,A] { def self = v; implicit def F: MonadZero[F] = F0 }

  ////

  ////
}

trait MonadZeroSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToMonadZeroOps[A](v: F[A]): MonadZeroOps[F, A] = new MonadZeroOps[F,A] { def self = v; implicit def F: MonadZero[F] = MonadZeroSyntax.this.F }

  def F: MonadZero[F]
  ////

  ////
}
