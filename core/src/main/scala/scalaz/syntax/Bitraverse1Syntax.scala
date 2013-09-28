package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bitraverse1` */
final class Bitraverse1Ops[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bitraverse1[F]) extends Ops[F[A, B]] {
  ////

  ////
}

sealed trait ToBitraverse1Ops0 {
    implicit def ToBitraverse1OpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bitraverse1, FA]) =
      new Bitraverse1Ops[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToBitraverse1Ops extends ToBitraverse1Ops0 with ToBitraverseOps with ToBifoldable1Ops {
  
  implicit def ToBitraverse1Ops[F[_, _],A, B](v: F[A, B])(implicit F0: Bitraverse1[F]) =
      new Bitraverse1Ops[F,A, B](v)
  

  
  implicit def ToBitraverse1VFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bitraverse1[({type λ[α, β]=F[G, α, β]})#λ]) =
        new Bitraverse1Ops[({type λ[α, β]=F[G, α, β]})#λ, A, B](v)(F0)

  ////

  ////
}

trait Bitraverse1Syntax[F[_, _]] extends BitraverseSyntax[F] with Bifoldable1Syntax[F] {
  implicit def ToBitraverse1Ops[A, B](v: F[A, B]): Bitraverse1Ops[F, A, B] = new Bitraverse1Ops[F, A, B](v)(Bitraverse1Syntax.this.F)

  def F: Bitraverse1[F]
  ////

  ////
}
