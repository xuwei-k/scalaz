package scalaz

////
/**
 * @see [[scalaz.Monad]]
 * @see [[scalaz.PlusEmpty]]
 */
////
trait MonadPlus[F[_]] extends MonadZero[F] with ApplicativePlus[F] { self =>
  ////

  /** Generalized version of Haskell's `catMaybes` */
  def unite[T[_], A](value: F[T[A]])(implicit T: Foldable[T]): F[A] =
    bind(value)((ta) => T.foldMap(ta)(a => point(a))(monoid[A]))

  trait MonadPlusLaw extends ApplicativePlusLaw with MonadZeroLaws {
  }

  trait StrongMonadPlusLaw extends MonadPlusLaw {
    /** `empty` short-circuits throughout its `join` tree. */
    def rightZero[A](f: F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(f)(_ => empty[A]), empty[A])
    }
  }
  def monadPlusLaw = new MonadPlusLaw {}
  def strongMonadPlusLaw = new StrongMonadPlusLaw {}
  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] { def F = MonadPlus.this }
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}
