package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrowInit` */
final class ArrowInitOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: ArrowInit[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToArrowInitOps0 {
  implicit def ToArrowInitOpsUnapply[FA](v: FA)(implicit F0: Unapply2[ArrowInit, FA]) =
    new ArrowInitOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToArrowInitOps extends ToArrowInitOps0 with ToArrowLoopOps {

  implicit def ToArrowInitOps[F[_, _],A, B](v: F[A, B])(implicit F0: ArrowInit[F]) =
    new ArrowInitOps[F,A, B](v)


  implicit def ToArrowInitVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: ArrowInit[F[G, ?, ?]]) =
    new ArrowInitOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ArrowInitSyntax[F[_, _]] extends ArrowLoopSyntax[F] {
  implicit def ToArrowInitOps[A, B](v: F[A, B]): ArrowInitOps[F, A, B] = new ArrowInitOps[F, A, B](v)(ArrowInitSyntax.this.F)

  def F: ArrowInit[F]
  ////

  ////
}
