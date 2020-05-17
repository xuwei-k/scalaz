package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `InvariantApplicative` */
final class InvariantApplicativeOps[F[_],A] private[syntax](val self: F[A])(implicit val F: InvariantApplicative[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToInvariantApplicativeOpsU[TC[F[_]] <: InvariantApplicative[F]] {
  implicit def ToInvariantApplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply[TC, FA]): InvariantApplicativeOps[F0.M, F0.A] =
    new InvariantApplicativeOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToInvariantApplicativeOps0[TC[F[_]] <: InvariantApplicative[F]] extends ToInvariantApplicativeOpsU[TC] {
  implicit def ToInvariantApplicativeOps[F[_],A](v: F[A])(implicit F0: TC[F]): InvariantApplicativeOps[F, A] =
    new InvariantApplicativeOps[F, A](v)

  ////

  ////
}

trait ToInvariantApplicativeOps[TC[F[_]] <: InvariantApplicative[F]] extends ToInvariantApplicativeOps0[TC] with ToInvariantFunctorOps[TC]

trait ToInvariantApplicativeOps2 extends ToInvariantFunctorOps2 {
  implicit def ToInvariantApplicativeOps2[F[_, _], A, B](v: F[A, B])(implicit F0: InvariantApplicative[({type l[x] = F[A, x]})#l]): InvariantApplicativeOps[({type l[x] = F[A, x]})#l, B] =
    new InvariantApplicativeOps[({type l[x] = F[A, x]})#l, B](v)
}

trait InvariantApplicativeSyntax[F[_]] extends InvariantFunctorSyntax[F] {
  implicit def ToInvariantApplicativeOps[A](v: F[A]): InvariantApplicativeOps[F, A] = new InvariantApplicativeOps[F,A](v)(InvariantApplicativeSyntax.this.F)

  def F: InvariantApplicative[F]
  ////

  ////
}
