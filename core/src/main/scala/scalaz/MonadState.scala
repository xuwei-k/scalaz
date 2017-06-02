package scalaz

////
/** The class of monads supporting the operations of
 * [[scalaz.State]].
 *
 */
////
trait MonadState[F[_], S] extends Monad[F] { self =>
  ////

  def get: F[S]
  def put(s: S): F[Unit]

  def state[A](f: S => (S, A)): F[A] = bind(init)(s => f(s) match { case (s, a) => bind(put(s))(_ => point(a)) })
  def constantState[A](a: A, s: => S): F[A] = bind(put(s))(_ => point(a))
  def init: F[S] = get
  def gets[A](f: S => A): F[A] = bind(init)(s => point(f(s)))
  def modify(f: S => S): F[Unit] = bind(init)(s => put(f(s)))

  trait MonadStateLaw extends MonadLaw {
    def putPut(s1: S, s2: S)(implicit E: Equal[F[Unit]]): Boolean =
      E.equal(bind(put(s1))(_ => put(s2)), put(s2))

    def putGet(s: S)(implicit E: Equal[F[S]]): Boolean =
      E.equal(bind(put(s))(_ => get), bind(put(s))(_ => point(s)))

    def getPut(implicit E: Equal[F[Unit]]): Boolean =
      E.equal(bind(get)(put), point(()))

    def getGet(k: (S, S) => F[Unit])(implicit E: Equal[F[Unit]]): Boolean =
      E.equal(bind(get)(s => bind(get)(k(s, _))), bind(get)(s => k(s, s)))
  }

  def monadStateLaw = new MonadStateLaw {}

  ////

}

object MonadState {
  @inline def apply[F[_], S](implicit F: MonadState[F, S]): MonadState[F, S] = F

  ////

  def promotedMonadState[G[_], F[_], S](
    implicit
    mpo: MonadPartialOrder[G, F],
    ms: MonadState[F, S]
  ): MonadState[G, S] = new MonadState[G, S] {

    override def get: G[S] = mpo.promote(ms.get)
    override def put(s: S): G[Unit] = mpo.promote(ms put s)

    override def point[A](a: => A): G[A] = mpo.MG point a
    override def bind[A, B](ga: G[A])(f: A => G[B]): G[B] = mpo.MG.bind(ga)(f)

  }

  ////
}
