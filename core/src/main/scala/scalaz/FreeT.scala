package scalaz

sealed abstract class FreeT[F[_], M[_], A]{
  import FreeT._

  final def run(implicit F: Functor[F], M: Monad[M]): M[FreeT.FreeF[F, A, FreeT[F, M, A]]] = this match {
    case Return(a) => a
    case Cont(a, f) =>
      M.bind(a.run){
        case Pure(a) => f(a).run
        case Free(w) => M.point(FreeT.Free(F.map(w)(_ flatMap f)))
      }
  }

  def flatMap[B](f: A => FreeT[F, M, B])(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] =
    Cont(this, f)
    
  import syntax.functor._

  def map[B](f: A => B)(implicit F: Functor[F], M: Monad[M]): FreeT[F, M, B] =
    FreeT(
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
        FreeT(G.map(a)(Pure(_)))
    }
}

object FreeT extends FreeTInstances {
  sealed abstract class FreeF[F[_], +A, +B]
  final case class Pure[F[_], A](a: A) extends FreeF[F, A, Nothing]
  final case class Free[F[_], B](w: F[B]) extends FreeF[F, Nothing, B]{

  }

  implicit def freeFEqual[F[_], A, B](implicit A: Equal[A], FB: Equal[F[B]]): Equal[FreeF[F, A, B]] =
    Equal.equal{ 
      case (Pure(a), Pure(b))             => A.equal(a, b)
      case (Free(a: F[B]), Free(b: F[B])) => FB.equal(a, b)
      case _                              => false
    }


  implicit def freeTEqual[F[_], M[_], A](implicit
    F: Functor[F],
    M: Monad[M],
    A: Equal[A],
    ME: Equal ~> ({type l[a] = Equal[M[FreeF[F, a, FreeT[F, M, a]]]]})#l
  ): Equal[FreeT[F, M, A]] = {
    Equal.equal{ (aa, bb) =>
      ME(A).equal(aa.run, bb.run) 
    }
  }

  trait Template[F[_], G[_]] extends (G ~> ({type λ[α] = G[F[α]]})#λ) {
    override final def apply[A](a: G[A]) = lift(a)

    def lift[A: G]: G[F[A]]
  }

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
          F.map(s)(x => FreeT(M point loop(x)))
        )
      case \/-(r) => Pure(r)
    }

    FreeT(M.map(m)(loop))
  }

  final case class Return[F[_], M[_], A](a: M[FreeT.FreeF[F, A, FreeT[F, M, A]]]) extends FreeT[F, M, A]
  final case class Cont[F[_], M[_], A, B](a: FreeT[F, M, B], f: B => FreeT[F, M, A]) extends FreeT[F, M, A]

  def apply[F[_], M[_], A](a: M[FreeT.FreeF[F, A, FreeT[F, M, A]]]): FreeT[F, M, A] = Return(a)
}
