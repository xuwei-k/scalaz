package scalaz

////
/**
 * Monad, an [[scalaz.Applicative]] that also supports [[scalaz.Bind]],
 * circumscribed by the monad laws.
 *
 * @see [[scalaz.Monad.MonadLaw]]
 */
////
trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  ////

  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => point(f(a)))

  trait MonadLaw extends ApplicativeLaw with BindLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
 }
  def monadLaw = new MonadLaw {}
  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] { def F = Monad.this }
}

object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  ////
}
