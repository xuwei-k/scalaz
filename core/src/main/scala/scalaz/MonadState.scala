package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_, _], S] extends Monad[F[S, ?]] { self =>
  ////

  def state[A](a: A): F[S, A] = bind(init)(s => point(a))
  def constantState[A](a: A, s: => S): F[S, A] = bind(put(s))(_ => point(a))
  def init: F[S, S]
  def get: F[S, S]
  def gets[A](f: S => A): F[S, A] = bind(init)(s => point(f(s)))
  def put(s: S): F[S, Unit]
  def modify(f: S => S): F[S, Unit] = bind(init)(s => put(f(s)))

  trait MonadStateLaw extends MonadLaw {
    def putPut(s1: S, s2: S)(implicit E: Equal[F[S, Unit]]): Boolean =
      E.equal(bind(put(s1))(_ => put(s2)), put(s2))

    def putGet(s: S)(implicit E: Equal[F[S, S]]): Boolean =
      E.equal(bind(put(s))(_ => get), bind(put(s))(_ => point(s)))

    def getPut(implicit E: Equal[F[S, Unit]]): Boolean =
      E.equal(bind(get)(put), point(()))

    def getGet(k: S => S => F[S, Unit])(implicit E: Equal[F[S, Unit]]): Boolean =
      E.equal(bind(get)(s => bind(get)(k(s))), bind(get)(s => k(s)(s)))
  }

  def monadStateLaw = new MonadStateLaw{}

  ////
  
}

object MonadState {
  @inline def apply[F[_, _], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  ////
}
