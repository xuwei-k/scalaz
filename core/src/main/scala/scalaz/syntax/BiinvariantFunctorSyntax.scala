package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiinvariantFunctor` */
final class BiinvariantFunctorOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: BiinvariantFunctor[F]) extends Ops[F[A, B]] {
  ////

  def bixmap[C, D](ac: A => C, ca: C => A, bd: B => D, db: D => B): F[C, D] = F.bixmap(self, ac, ca, bd, db)

  ////
}

sealed trait ToBiinvariantFunctorOps0 {
    implicit def ToBiinvariantFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply2[BiinvariantFunctor, FA]) =
      new BiinvariantFunctorOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)
  
}

trait ToBiinvariantFunctorOps extends ToBiinvariantFunctorOps0 {
  
  implicit def ToBiinvariantFunctorOps[F[_, _],A, B](v: F[A, B])(implicit F0: BiinvariantFunctor[F]) =
      new BiinvariantFunctorOps[F,A, B](v)
  

  
  implicit def ToBiinvariantFunctorVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: BiinvariantFunctor[({type λ[α, β]=F[G, α, β]})#λ]) =
        new BiinvariantFunctorOps[({type λ[α, β]=F[G, α, β]})#λ, A, B](v)(F0)

  ////

  ////
}

trait BiinvariantFunctorSyntax[F[_, _]]  {
  implicit def ToBiinvariantFunctorOps[A, B](v: F[A, B]): BiinvariantFunctorOps[F, A, B] = new BiinvariantFunctorOps[F, A, B](v)(BiinvariantFunctorSyntax.this.F)

  def F: BiinvariantFunctor[F]
  ////

  ////
}
