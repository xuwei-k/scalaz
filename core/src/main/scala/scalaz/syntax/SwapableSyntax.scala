package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Swapable` */
trait SwapableOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Swapable[F]
  ////

  ////
}

trait ToSwapableOps0 {
  implicit def ToSwapableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Swapable, FA]) =
    new SwapableOps[F0.M,F0.A] { def self = F0(v); implicit def F: Swapable[F0.M] = F0.TC }

}

trait ToSwapableOps extends ToSwapableOps0 with ToMonadOps {
  implicit def ToSwapableOps[F[_],A](v: F[A])(implicit F0: Swapable[F]) =
    new SwapableOps[F,A] { def self = v; implicit def F: Swapable[F] = F0 }

  ////

  ////
}

trait SwapableSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToSwapableOps[A](v: F[A]): SwapableOps[F, A] = new SwapableOps[F,A] { def self = v; implicit def F: Swapable[F] = SwapableSyntax.this.F }

  def F: Swapable[F]
  ////

  ////
}
