package scalaz

////
/**
 *
 */
////
trait Bitraverse1[F[_, _]] extends Bitraverse[F] with Bifoldable1[F] { self =>
  ////

  def bitraverse1Impl[G[_] : Apply, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  // derived functions

  override def bitraverseImpl[G[_] : Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]] =
    bitraverse1Impl(fab)(f, g)

  ////
  val bitraverse1Syntax = new scalaz.syntax.Bitraverse1Syntax[F] { def F = Bitraverse1.this }
}

object Bitraverse1 {
  @inline def apply[F[_, _]](implicit F: Bitraverse1[F]): Bitraverse1[F] = F

  ////

  ////
}
