package scalaz

final case class FreeT[F[_], M[_], A](run: M[FreeT.FreeF[F, A, FreeT[F, M, A]]]){
  import FreeT._

  def flatMap[B](f: A => FreeT[F, M, B])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] =
    new FreeT(
      M.bind(run){
        case Pure(a) => f(a).run
        case Free(w) => M.point(FreeT.Free(F.map(w)(_ flatMap f)))
      }
    )

  import syntax.functor._

  def map[B](f: A => B)(implicit F: Functor[F], M: Functor[M]): FreeT[F, M, B] =
    new FreeT(
      M.map(run){
        case Pure(a) => Pure(f(a))
        case Free(w) => Free(w.map(_.map(f)))
      }
    )
}


sealed abstract class FreeTInstances {
  import FreeT._

  implicit def freeTMonad[F[_], M[_]](implicit F: Functor[F], M: Monad[M]): Monad[({type λ[α]=FreeT[F, M, α]})#λ] =
    new Monad[({type λ[α]=FreeT[F, M, α]})#λ] {
      def point[A](a: => A) =
        FreeT(M.point(FreeT.Pure(a)))

      def bind[A, B](fa: FreeT[F, M, A])(f: A => FreeT[F, M, B]) =
        fa flatMap f

      override def map[A, B](fa: FreeT[F, M, A])(f: A => B) =
        fa map f
    }

  implicit def freeTMonadTrans[F[_]: Functor]: MonadTrans[({type λ[α[_], β]=FreeT[F, α, β]})#λ] =
    new MonadTrans[({type λ[α[_], β]=FreeT[F, α, β]})#λ] {
      implicit def apply[G[_]: Monad] =
        freeTMonad[F, G]

      def liftM[G[_], A](a: G[A])(implicit G: Monad[G]) =
        new FreeT(G.map(a)(Pure(_)))
    }
}

object FreeT extends FreeTInstances {
  sealed abstract class FreeF[F[_], +A, +B]
  final case class Pure[F[_], A](a: A) extends FreeF[F, A, Nothing]
  final case class Free[F[_], B](w: F[B]) extends FreeF[F, Nothing, B]

  implicit def freeFFunctor[F[_], X](implicit F: Functor[F]): Functor[({type λ[α]=FreeF[F, X, α]})#λ] =
    new Functor[({type λ[α]=FreeF[F, X, α]})#λ]{
      override def map[A, B](fa: FreeF[F, X, A])(f: A => B): FreeF[F, X, B] = fa match {
        case a @ Pure(_) => a
        case Free(w) => Free(F.map(w)(f))
      }
    }

  import scalaz.{Free => ScalazFree}

  def fromFree[F[_], M[_], A](m: M[scalaz.Free[F, A]])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, A] = {
    def loop(f: scalaz.Free[F, A]): FreeF[F, A, FreeT[F, M, A]] = f.resume match{
      case -\/(s) =>
        Free(
          F.map(s)( x => new FreeT(M point loop(x)))
        )
      case \/-(r) => Pure(r)
    }

    new FreeT(M.map(m)(loop))
  }
}
