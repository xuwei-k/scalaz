package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Witherable` */
final class WitherableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Witherable[F]) extends Ops[F[A]] {
  ////

  import scalaz.Leibniz.===

  def wither[G[_]: Applicative, B](f: A => G[Maybe[B]]): G[F[B]] =
    F.wither(self)(f)

  def mapMaybe[B](f: A => Maybe[B]): F[B] =
    F.mapMaybe(self)(f)

  def catMaybes[B](implicit ev: A === Maybe[B]): F[B] =
    F.catMaybes(ev.subst(self))

  def filterA[G[_]: Applicative](f: A => G[Boolean]): G[F[A]] =
    F.filterA(self)(f)

  def filterW(f: A => Boolean): F[A] =
    F.filterW(self)(f)

  ////
}

sealed trait ToWitherableOps0 {
  implicit def ToWitherableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Witherable, FA]) =
    new WitherableOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToWitherableOps extends ToWitherableOps0 with ToTraverseOps {
  implicit def ToWitherableOps[F[_],A](v: F[A])(implicit F0: Witherable[F]) =
    new WitherableOps[F,A](v)

  ////

  ////
}

trait WitherableSyntax[F[_]] extends TraverseSyntax[F] {
  implicit def ToWitherableOps[A](v: F[A]): WitherableOps[F, A] = new WitherableOps[F,A](v)(WitherableSyntax.this.F)

  def F: Witherable[F]
  ////

  ////
}
