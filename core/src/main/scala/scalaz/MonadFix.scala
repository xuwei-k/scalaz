package scalaz

////
/**
 * @see [[http://hackage.haskell.org/packages/archive/base/latest/doc/html/Control-Monad-Fix.html]]
 */
////
trait MonadFix[F[_]] extends Monad[F] { self =>
  ////

  def mfix[A](f: (=> A) => F[A]): F[A]

  // derived functions

  final def fix[A](f: (=> A) => A): A = {
    lazy val a: A = f(a)
    a
  }

  trait MonadFixLaw extends MonadLaw {
    import syntax.std.function1._
    private[this] def p[A]: A => F[A] = x => point(x) 

    // mfix (return . h) = return (fix h)
    def purity[B](f: (=> B) => B)(implicit E: Equal[F[B]]): Boolean = {
      E.equal(mfix(p compose f), point(fix[B](f)))
    }

    // mfix (\x -> a >>= \y -> f x y)    =     a >>= \y -> mfix (\x -> f x y)
    def tightening[A, B](a: F[A], f: (B, A) => F[B])(implicit E: Equal[F[B]]): Boolean =
      E.equal(
        mfix(x => bind(a){y => f(x, y)}),
        bind(a){y => mfix{x => f(x, y)}}
      )

    // mfix (liftM h . f)   =   liftM h (mfix (f . h))
    def sliding[A, B](f: A => F[B], h: B => A)(implicit E: Equal[F[A]]): Boolean = {

      E.equal(
        mfix{x => lift(h){f(x)} },
        lift(h)(mfix(f compose h.byName))
      )
    }

    // mfix (\x -> mfix (\y -> f x y)) = mfix (\x -> f x x)
    def nesting[A](f: (A, A) => F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(
        mfix(x => mfix(y => f(x, y))),
        mfix(x => f(x, x))
      )

  }
  def monadFixLaw = new MonadFixLaw{}

  ////
  val monadFixSyntax = new scalaz.syntax.MonadFixSyntax[F] { def F = MonadFix.this }
}

object MonadFix {
  @inline def apply[F[_]](implicit F: MonadFix[F]): MonadFix[F] = F

  ////

  ////
}
