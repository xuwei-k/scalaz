package scalaz

trait Codensity[F[_], A] { self =>
  def apply[B](f: A => F[B]): F[B]
  def improve(implicit F: Applicative[F]): F[A] =
    apply(a => F.point(a))
  def flatMap[B](k: A => Codensity[F, B]): Codensity[F, B] = {
    new Codensity[F, B] {
      def apply[C](h: B => F[C]): F[C] = 
        self.apply(a => k(a)(h))
    }
  }
  def map[B](k: A => B): Codensity[F, B] =
    flatMap(x => Codensity.pureCodensity(k(x)))
}

object Codensity {
  def rep[F[_], A](f: F[A])(implicit F: Bind[F]): Codensity[F, A] =
    new Codensity[F, A] {
      def apply[B](k: A => F[B]) = F.bind(f)(k)
    }
  def pureCodensity[F[_], A](a: => A): Codensity[F, A] = new Codensity[F, A] {
    def apply[B](f: A => F[B]): F[B] = f(a)
  }

  implicit def codensityInstance[F[_]]: Monad[({type λ[α]=Codensity[F, α]})#λ] =
    new Monad[({type λ[α]=Codensity[F, α]})#λ] {
      def point[A](a: => A) =
        pureCodensity(a)

      def bind[A, B](fa: Codensity[F, A])(f: A => Codensity[F, B]) =
        fa flatMap f
    }

  implicit val codensityMoandTrans: MonadTrans[Codensity] =
    new MonadTrans[Codensity] {
      implicit def apply[G[_]: Monad] =
        codensityInstance[G]

      def liftM[G[_]: Monad, A](a: G[A]) =
        rep(a)
    }
}
