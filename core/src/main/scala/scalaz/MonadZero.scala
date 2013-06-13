package scalaz

////
/**
 *
 */
////
trait MonadZero[F[_]] extends Monad[F] { self =>
  ////
  def empty[A]: F[A]

  // derived functions

  /** Remove `f`-failing `A`s in `fa`, by which we mean: in the
    * expression `filter(filter(fa)(f))(g)`, `g` will never be invoked
    * for any `a` where `f(a)` returns false.
    */
  def filter[A](fa: F[A])(f: A => Boolean) =
    bind(fa)(a => if (f(a)) point(a) else empty[A])

  trait MonadZeroLaws extends MonadLaw {
    /** `empty` short-circuits its right. */
    def leftZero[A](f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(empty[A])(f), empty[A])
    }
  }
  def monadZeroLaw = new MonadZeroLaws {}
  ////
  val monadZeroSyntax = new scalaz.syntax.MonadZeroSyntax[F] { def F = MonadZero.this }
}

object MonadZero {
  @inline def apply[F[_]](implicit F: MonadZero[F]): MonadZero[F] = F

  ////

  ////
}
