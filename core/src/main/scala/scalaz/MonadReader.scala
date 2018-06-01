package scalaz

////
/**
 *
 */
////
trait MonadReader[F[_], S] extends Monad[F] { self =>
  ////

  def ask: F[S]
  def local[A](f: S => S)(fa: F[A]): F[A]
  def scope[A](k: S)(fa: F[A]): F[A] = local(_ => k)(fa)
  def asks[A](f: S => A): F[A] = map(ask)(f)

////

}

object MonadReader {
  @inline def apply[F[_], S](implicit F: MonadReader[F, S]): MonadReader[F, S] = F

  ////

////
}

trait IsomorphismMonadReader[F[_], G[_], S] extends MonadReader[F, S] with IsomorphismMonad[F, G]{
  implicit def G: MonadReader[G, S]
////

  override def ask: F[S] = iso.from(G.ask)

  override def local[A](f: S => S)(fa: F[A]): F[A] =
    iso.from(G.local(f)(iso.to(fa)))
////
}
