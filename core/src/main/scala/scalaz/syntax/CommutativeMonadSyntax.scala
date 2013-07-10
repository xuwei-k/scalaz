package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CommutativeMonad` */
trait CommutativeMonadOps[F[_],A] extends Ops[F[A]] {
  implicit def F: CommutativeMonad[F]
  ////

  ////
}

trait ToCommutativeMonadOps0 {
  implicit def ToCommutativeMonadOpsUnapply[FA](v: FA)(implicit F0: Unapply[CommutativeMonad, FA]) =
    new CommutativeMonadOps[F0.M,F0.A] { def self = F0(v); implicit def F: CommutativeMonad[F0.M] = F0.TC }

}

trait ToCommutativeMonadOps extends ToCommutativeMonadOps0 with ToMonadOps {
  implicit def ToCommutativeMonadOps[F[_],A](v: F[A])(implicit F0: CommutativeMonad[F]) =
    new CommutativeMonadOps[F,A] { def self = v; implicit def F: CommutativeMonad[F] = F0 }

  ////

  ////
}

trait CommutativeMonadSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToCommutativeMonadOps[A](v: F[A]): CommutativeMonadOps[F, A] = new CommutativeMonadOps[F,A] { def self = v; implicit def F: CommutativeMonad[F] = CommutativeMonadSyntax.this.F }

  def F: CommutativeMonad[F]
  ////

  ////
}
