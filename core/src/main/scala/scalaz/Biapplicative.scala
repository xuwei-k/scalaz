package scalaz

////
/**
 *
 */
////
trait Biapplicative[F[_, _]] extends Bifunctor[F] { self =>
  ////

  // derived functions
  override def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D] = biap(fab)(bipure(f, g))

  /**The composition of Biapplicatives `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Biapplicative */
  def compose[G[_, _]](implicit G0: Biapplicative[G]): Biapplicative[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] = new CompositionBiapplicative[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  def bipure[A, B](a: A, b: B): F[A, B]

  def biap[A, B, C, D](fac: => F[A, C])(fabcd: => F[A => B, C => D]): F[B, D]

  def biapply2[A, B, C, D](fab: => F[A, B], fcd: => F[C, D])(f: (A, C) => C, g: (B, D) => D): F[C, D] = biap(fcd)(bimap(fab)(f.curried, g.curried))

  ////
  val biapplicativeSyntax = new scalaz.syntax.BiapplicativeSyntax[F] { def F = Biapplicative.this }
}

object Biapplicative {
  @inline def apply[F[_, _]](implicit F: Biapplicative[F]): Biapplicative[F] = F

  ////

  ////
}
