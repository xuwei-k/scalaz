package scalaz

////
/**
 *
 */
////
trait Biapplicative[F[_, _]] extends Bifunctor[F] { self =>
  ////

  // derived functions
  def bipure[A, B](a: A, b: B): F[A, B]

  def biap[A, B, C, D](fac: => F[A, C])(fabcd: => F[A => B, C => D]): F[B, D]

  def biapply2[A, B, C, D](fab: => F[A, B], fcd: => F[C, D])(f: A => C => C, g: B => D => D): F[C, D] = biap(fcd)(bimap(fab)(f, g))

  ////
  val biapplicativeSyntax = new scalaz.syntax.BiapplicativeSyntax[F] { def F = Biapplicative.this }
}

object Biapplicative {
  @inline def apply[F[_, _]](implicit F: Biapplicative[F]): Biapplicative[F] = F

  ////

  ////
}
