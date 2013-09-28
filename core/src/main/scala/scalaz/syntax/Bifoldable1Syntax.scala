package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifoldable1` */
final class Bifoldable1Ops[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bifoldable1[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToBifoldable1Ops0 {
    implicit def ToBifoldable1OpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifoldable1, FA]) =
      new Bifoldable1Ops[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToBifoldable1Ops extends ToBifoldable1Ops0 with ToBifoldableOps {
  
  implicit def ToBifoldable1Ops[F[_, _],A, B](v: F[A, B])(implicit F0: Bifoldable1[F]) =
      new Bifoldable1Ops[F,A, B](v)
  

  
  implicit def ToBifoldable1VFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bifoldable1[({type λ[α, β]=F[G, α, β]})#λ]) =
        new Bifoldable1Ops[({type λ[α, β]=F[G, α, β]})#λ, A, B](v)(F0)

  ////

  ////
}

trait Bifoldable1Syntax[F[_, _]] extends BifoldableSyntax[F] {
  implicit def ToBifoldable1Ops[A, B](v: F[A, B]): Bifoldable1Ops[F, A, B] = new Bifoldable1Ops[F, A, B](v)(Bifoldable1Syntax.this.F)

  def F: Bifoldable1[F]
  ////

  ////
}
