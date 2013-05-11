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

  def composedMonad[G[_]](implicit g: Swapable[G]): Monad[({type λ[α] = F[G[α]]})#λ] = new ComposedMonad[F, G]{
    implicit val G: Swapable[G] = g
    implicit val F: Monad[F] = self
  }

  trait MonadLaw extends ApplicativeLaw {
    /** Lifted `point` is a no-op. */
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
    /** Lifted `f` applied to pure `a` is just `f(a)`. */
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
    /**
     * As with semigroups, monadic effects only change when their
     * order is changed, not when the order in which they're
     * combined changes.
     */
    def associativeBind[A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(bind(bind(fa)(f))(g), bind(fa)((a: A) => bind(f(a))(g)))
    /** `ap` is consistent with `bind`. */
    def apLikeDerived[A, B](fa: F[A], f: F[A => B])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(ap(fa)(f), bind(f)(f => map(fa)(f)))
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

private[scalaz] trait ComposedMonad[F[_], G[_]] extends Monad[({type λ[α] = F[G[α]]})#λ]{
  implicit val F: Monad[F]
  implicit val G: Swapable[G]

  override def point[A](a: => A) = F.point(G.point(a))
  override def map[A, B](fa: F[G[A]])(f: A => B) = F.map(fa)(G.map(_)(f))
  override def bind[A, B](fa: F[G[A]])(f: A => F[G[B]]) = join(map(fa)(f))
  override def join[A](fgfga: F[G[F[G[A]]]]) =
    F.map(
      F.join(F.map(fgfga)(G.swap(_)))
    )(G.join(_))
}
