package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadRec` */
final class MonadRecOps[F[_],A] private[syntax](val self: F[A])(implicit val F: MonadRec[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToMonadRecOps0 {
  implicit def ToMonadRecOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadRec, FA]) =
    new MonadRecOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadRecOps extends ToMonadRecOps0 with ToBindRecOps with ToMonadOps {
  implicit def ToMonadRecOps[F[_],A](v: F[A])(implicit F0: MonadRec[F]) =
    new MonadRecOps[F,A](v)

  ////

  ////
}

trait MonadRecSyntax[F[_]] extends BindRecSyntax[F] with MonadSyntax[F] {
  implicit def ToMonadRecOps[A](v: F[A]): MonadRecOps[F, A] = new MonadRecOps[F,A](v)(MonadRecSyntax.this.F)

  def F: MonadRec[F]
  ////

  ////
}
