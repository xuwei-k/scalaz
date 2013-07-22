package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadPlus` */
final class MonadPlusOps[F[_],A] private[scalaz] (override val self: F[A])(implicit private[this] val F: MonadPlus[F]) extends Ops[F[A]] {
  ////
  import Liskov._

  def filter(f: A => Boolean) =
    F.filter(self)(f)
  
  def withFilter(f: A => Boolean) =
    filter(f)

  def unite[T[_], B](implicit ev: A <~< T[B], T: Foldable[T]): F[B] = {
    val ftb: F[T[B]] = Liskov.co[F, A, T[B]](ev)(self)
    F.unite[T, B](ftb)
  }

  ////
}

trait ToMonadPlusOps0 {
  implicit final def ToMonadPlusOpsUnapply[FA](v: FA)(implicit F0: Unapply[MonadPlus, FA]) =
    new MonadPlusOps[F0.M,F0.A](F0(v))(F0.TC)

}

trait ToMonadPlusOps extends ToMonadPlusOps0 with ToMonadOps with ToApplicativePlusOps {
  implicit final def ToMonadPlusOps[F[_],A](v: F[A])(implicit F0: MonadPlus[F]) =
    new MonadPlusOps[F,A](v)

  ////

  ////
}

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with ApplicativePlusSyntax[F] {
  implicit final def ToMonadPlusOps[A](v: F[A]): MonadPlusOps[F, A] = new MonadPlusOps[F,A](v)(MonadPlusSyntax.this.F)

  def F: MonadPlus[F]
  ////

  ////
}
