package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrowLoop` */
final class ArrowLoopOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: ArrowLoop[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToArrowLoopOps0 {
  implicit def ToArrowLoopOpsUnapply[FA](v: FA)(implicit F0: Unapply2[ArrowLoop, FA]) =
    new ArrowLoopOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToArrowLoopOps extends ToArrowLoopOps0 with ToArrowOps {

  implicit def ToArrowLoopOps[F[_, _],A, B](v: F[A, B])(implicit F0: ArrowLoop[F]) =
    new ArrowLoopOps[F,A, B](v)


  implicit def ToArrowLoopVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: ArrowLoop[F[G, ?, ?]]) =
    new ArrowLoopOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ArrowLoopSyntax[F[_, _]] extends ArrowSyntax[F] {
  implicit def ToArrowLoopOps[A, B](v: F[A, B]): ArrowLoopOps[F, A, B] = new ArrowLoopOps[F, A, B](v)(ArrowLoopSyntax.this.F)

  def F: ArrowLoop[F]
  ////

  ////
}
