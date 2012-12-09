package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Monoidal` */
trait MonoidalOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Monoidal[F]
  ////

  ////
}

trait ToMonoidalOps0 {
  implicit def ToMonoidalOpsUnapply[FA](v: FA)(implicit F0: Unapply[Monoidal, FA]) =
    new MonoidalOps[F0.M,F0.A] { def self = F0(v); implicit def F: Monoidal[F0.M] = F0.TC }

}

trait ToMonoidalOps extends ToMonoidalOps0 with ToFunctorOps {
  implicit def ToMonoidalOps[F[_],A](v: F[A])(implicit F0: Monoidal[F]) =
    new MonoidalOps[F,A] { def self = v; implicit def F: Monoidal[F] = F0 }

  ////

  ////
}

trait MonoidalSyntax[F[_]] extends FunctorSyntax[F] { 
  implicit def ToMonoidalOps[A](v: F[A]): MonoidalOps[F, A] = new MonoidalOps[F,A] { def self = v; implicit def F: Monoidal[F] = MonoidalSyntax.this.F }

  def F: Monoidal[F]
  ////

  ////
}
