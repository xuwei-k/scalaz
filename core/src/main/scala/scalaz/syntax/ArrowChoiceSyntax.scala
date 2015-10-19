package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrowChoice` */
final class ArrowChoiceOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: ArrowChoice[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToArrowChoiceOps0 {
    implicit def ToArrowChoiceOpsUnapply[FA](v: FA)(implicit F0: Unapply2[ArrowChoice, FA]) =
      new ArrowChoiceOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToArrowChoiceOps extends ToArrowChoiceOps0 with ToArrowOps with ToProChoiceOps {
  
  implicit def ToArrowChoiceOps[F[_, _],A, B](v: F[A, B])(implicit F0: ArrowChoice[F]) =
    new ArrowChoiceOps[F,A, B](v)
  

  
  implicit def ToArrowChoiceVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: ArrowChoice[F[G, ?, ?]]) =
    new ArrowChoiceOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait ArrowChoiceSyntax[F[_, _]] extends ArrowSyntax[F] with ProChoiceSyntax[F] {
  implicit def ToArrowChoiceOps[A, B](v: F[A, B]): ArrowChoiceOps[F, A, B] = new ArrowChoiceOps[F, A, B](v)(ArrowChoiceSyntax.this.F)

  def F: ArrowChoice[F]
  ////

  ////
}
