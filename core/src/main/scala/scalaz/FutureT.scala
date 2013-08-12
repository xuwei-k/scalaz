package scalaz

import scala.concurrent.duration.Duration
import scala.concurrent.{Future, ExecutionContext, Await}

final case class FutureT[F[_], A](run: F[Future[A]])(implicit E: ExecutionContext) { self =>

  def map[B](f: A => B)(implicit F: Functor[F]): FutureT[F, B] =
    new FutureT[F, B](mapO(_ map f))

  def flatMap[B](f: A => FutureT[F, B])(implicit F: Monad[F]): FutureT[F, B] =
    new FutureT[F, B](
      F.bind(self.run) { a =>
        f(Await.result(a, Duration.Inf)).run // TODO correct ?
      }
    )

  @inline private[this] def mapO[B](f: Future[A] => B)(implicit F: Functor[F]) =
    F.map(run)(f)
}

object FutureT{
  implicit def futureTMonad[F[_]](implicit F: Monad[F], E: ExecutionContext): Monad[({type λ[α] = FutureT[F, α]})#λ] =
    new Monad[({type λ[α] = FutureT[F, α]})#λ] {
      def point[A](a: => A) =
        FutureT(F point Future(a))

      def bind[A, B](fa: FutureT[F, A])(f: A => FutureT[F, B]) =
        fa flatMap f
    }

  implicit def futureTHoist(implicit E: ExecutionContext): Hoist[FutureT] = new Hoist[FutureT] {
    def hoist[M[_]: Monad, N[_]](f: M ~> N) =
      new (({type λ[α] = FutureT[M, α]})#λ ~> ({type λ[α] = FutureT[N, α]})#λ) {
        def apply[A](fa: FutureT[M, A]) = FutureT(f(fa.run))
      }

    implicit def apply[G[_]: Monad] =
      futureTMonad[G](implicitly, E)

    def liftM[G[_], A](a: G[A])(implicit G: Monad[G]): FutureT[G,A] =
      FutureT[G, A](G.map(a)(Future(_)(E)))(E)
  }
}
