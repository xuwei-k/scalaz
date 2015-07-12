package scalaz

////
/**
 * @see [[http://hackage.haskell.org/package/witherable]]
 */
////
trait Witherable[F[_]] extends Traverse[F] { self =>
  ////

  import scalaz.Id.Id

  def wither[G[_], A, B](fa: F[A])(f: A => G[Maybe[B]])(implicit G: Applicative[G]): G[F[B]]

  def mapMaybe[A, B](fa: F[A])(f: A => Maybe[B]): F[B] =
    wither[Id, A, B](fa)(f)

  def catMaybes[A](fa: F[Maybe[A]]): F[A] =
    mapMaybe(fa)(x => x)

  def filterA[G[_], A](fa: F[A])(f: A => G[Boolean])(implicit G: Applicative[G]): G[F[A]] =
    wither(fa)(a =>
      G.map(f(a))(b =>
        if(b) Maybe.just(a) else Maybe.empty[A]
      )
    )

  def filterW[A](fa: F[A])(f: A => Boolean): F[A] =
    filterA[Id, A](fa)(f)


  trait WitherableLaw extends TraverseLaw {
    def witherableIdentity[G[_], A](fa: F[A])(implicit G: Applicative[G], E: Equal[G[F[A]]]) =
      E.equal(wither(fa)(a => G.point(Maybe.just(a))), G.point(fa))

    // TODO
    // Compose . fmap ('wither' f) . 'wither' g ≡ 'wither' (Compose . fmap ('wither' f) . g)
    //def witherableComposition
  }

  def witherableLaw = new WitherableLaw {}

  ////
  val witherableSyntax = new scalaz.syntax.WitherableSyntax[F] { def F = Witherable.this }
}

object Witherable {
  @inline def apply[F[_]](implicit F: Witherable[F]): Witherable[F] = F

  ////

/*
  import scalaz.Id.Id

  type FilterLike[F[_], S, T, A, B] = (A => F[Maybe[B]]) => S => F[T]
  type FilterLike0[F[_], S, A] = FilterLike[F, S, S, A, A]

  def mapMaybeOf[S, T, A, B](w: FilterLike[Id, S, T, A, B])(f: A => Maybe[B]): S => T =
    w(f)

  def filterAOf[F[_], A, S](w: (A => F[Maybe[A]]) => S => F[S])(f: A => F[Boolean])(implicit F: Functor[F]): S => F[S] =
    w(a =>
      F.map(f(a)){ b =>
        if(b) Maybe.just(a) else Maybe.empty[A]
      }
    )

  def filterOf[A, S](w: (A => Maybe[A]) => S => S)(f: A => Boolean): S => S =
    filterAOf[Id, A, S](w)(f)
  */


////
}
