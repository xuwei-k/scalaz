package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Commutative` */
final class CommutativeOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Commutative[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCommutativeOps0 {
  implicit def ToCommutativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[Commutative, FA]) =
    new CommutativeOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToCommutativeOps extends ToCommutativeOps0 with ToMonadOps {
  implicit def ToCommutativeOps[F[_],A](v: F[A])(implicit F0: Commutative[F]) =
    new CommutativeOps[F,A](v)

  ////

  ////
}

trait CommutativeSyntax[F[_]] extends MonadSyntax[F] {
  implicit def ToCommutativeOps[A](v: F[A]): CommutativeOps[F, A] = new CommutativeOps[F,A](v)(CommutativeSyntax.this.F)

  def F: Commutative[F]
  ////

  ////
}
