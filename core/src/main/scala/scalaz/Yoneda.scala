package scalaz

/**
 * The cofree functor generated by `F`. The Yoneda lemma says that
 * `Yoneda[F,A]` is isomorphic to `F[A]` for any functor `F`.
 * The homomorphism from `Yoneda[F,A]` to `F[A]` exists even when
 * we have forgotten that `F` is a functor.
 * Can be seen as a partially applied `map` for the functor `F`.
 */
abstract class Yoneda[F[_], A] { yo =>
  def apply[B](f: A => B): F[B]

  /** Converts to `F[A]` even without a `Functor` instance for `F` */
  def run: F[A] = apply(a => a)

  /** Converts to `Coyoneda[F,A]` even without a `Functor` instance for `F` */
  def toCoyoneda: Coyoneda.Aux[F,A,A] = Coyoneda(run)(identity[A])

  /** Simple function composition. Allows map fusion without traversing an `F`. */
  def map[B](f: A => B): Yoneda[F, B] =
    new Yoneda[F, B] {
      def apply[C](g: B => C) = yo(f.andThen(g))
    }

  import Id._
  /** `Yoneda[F, _]` is the right Kan extension of `F` along `Id` */
  def toRan: Ran[Id, F, A] =
    new Ran[Id, F, A] {
      def apply[B](f: A => B) = yo(f)
    }

  /** `Yoneda` is a comonad in an endofunctor category */
  def extend[G[_]:Functor](f: Yoneda[F,*] ~> G): Yoneda[G,A] =
    Yoneda(f(this))

  /** `Yoneda` is a monad in an endofunctor category */
  def flatMap[G[_]](f: F ~> Yoneda[G,*]): Yoneda[G,A] =
    f(run)
}

object Yoneda {

  /** `Yoneda[F,_]` is a functor for any `F` */
  implicit def yonedaFunctor[F[_]]: Functor[Yoneda[F, *]] =
    new Functor[Yoneda[F, *]] {
      def map[A,B](ya: Yoneda[F,A])(f: A => B) = ya.map(f)
    }

  /** `F[A]` converts to `Yoneda[F,A]` for any functor `F` */
  def apply[F[_]:Functor,A](fa: F[A]): Yoneda[F, A] =
    new Yoneda[F, A] {
      def apply[B](f: A => B) = Functor[F].map(fa)(f)
    }
}

