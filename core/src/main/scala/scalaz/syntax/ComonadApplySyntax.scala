package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ComonadApply` */
trait ComonadApplyOps[F[_],A] extends Ops[F[A]] {
  implicit def F: ComonadApply[F]
  ////

  def <@>[B](fab: F[A => B]): F[B] = F.<@>(self)(fab)

  def @>[B](fb: F[B]): F[B] = F.@>(self)(fb)

  def <@[B](fb: F[B]): F[A] = F.<@(self)(fb)

  ////
}

trait ToComonadApplyOps0 {
  implicit def ToComonadApplyOpsUnapply[FA](v: FA)(implicit F0: Unapply[ComonadApply, FA]) =
    new ComonadApplyOps[F0.M,F0.A] { def self = F0(v); implicit def F: ComonadApply[F0.M] = F0.TC }

}

trait ToComonadApplyOps extends ToComonadApplyOps0 with ToComonadOps {
  implicit def ToComonadApplyOps[F[_],A](v: F[A])(implicit F0: ComonadApply[F]) =
    new ComonadApplyOps[F,A] { def self = v; implicit def F: ComonadApply[F] = F0 }

  ////

  ////
}

trait ComonadApplySyntax[F[_]] extends ComonadSyntax[F] {
  implicit def ToComonadApplyOps[A](v: F[A]): ComonadApplyOps[F, A] = new ComonadApplyOps[F,A] { def self = v; implicit def F: ComonadApply[F] = ComonadApplySyntax.this.F }

  def F: ComonadApply[F]
  ////

  ////
}
