package scalaz

import annotation.tailrec
import std.function._
import Free.Trampoline
import TrampolineT._

object TrampolineT extends TrampolineTInstances {

  private final case class Done[M[_], A](a: M[A]) extends TrampolineT[M, A]
  private final case class More[M[_], A](a: M[Function0[TrampolineT[M, A]]]) extends TrampolineT[M, A]
  private final case class FlatMap[M[_], A, B](a: TrampolineT[M, B], f: B => TrampolineT[M, A]) extends TrampolineT[M, A]

  def from[M[_], A](m: M[Trampoline[A]])(implicit M: Applicative[M]): TrampolineT[M, A] = {
    def loop(f: Trampoline[A]): TrampolineT[M, A] = f.resume match{
      case -\/(s) =>
        More(M.point(Functor[Function0].map(s)(loop)))
      case \/-(r) =>
        Done(M.point(r))
    }

    More(M.map(m)(x => () => loop(x)))
  }

  def done[M[_], A](a: M[A]): TrampolineT[M, A] =
    Done(a)

  def delay[M[_], A](a: => M[A])(implicit M: Applicative[M]): TrampolineT[M, A] =
    More(M.point(() => Done(a)))

  def more[M[_], A](a: M[Function0[TrampolineT[M, A]]]): TrampolineT[M, A] =
    More(a)

  def suspend[M[_], A](a: => TrampolineT[M, A])(implicit M: Applicative[M]): TrampolineT[M, A] =
    More(M.point(() => a))

  def bind[M[_], A, B](a: TrampolineT[M, B])(f: B => TrampolineT[M, A]): TrampolineT[M, A] =
    FlatMap(a, f)
}

/** Trampline Monad Transformer */
sealed abstract class TrampolineT[M[_], A] {

  final def toTrampoline(implicit M: Bind[M], T: Traverse[M]): Trampoline[M[A]] =
    resume(M) match {
      case \/-(a) => Trampoline.done(a)
      case -\/(a) =>
        val F = Bind[Trampoline]
        F.join(
          F.map(
            T.traverse(a)(x => Trampoline.delay(x().toTrampoline))
          )(z => F.map(T.sequence(z))(M.join))
        )
    }

  final def run(implicit M: Bind[M], T: Traverse[M]): M[A] =
    toTrampoline.run

  @tailrec
  final def resume(implicit M: Functor[M]): M[Function0[TrampolineT[M, A]]] \/ M[A] =
    this match {
      case Done(a)       => \/-(a)
      case More(a)       => -\/(a)
      case FlatMap(a, f) => a match {
        case Done(b)       => -\/(M.map(b)(x => () => f(x)))
        case More(b)       => -\/(M.map(b)(x => Functor[Function0].map(x)(_ flatMap f)))
        case FlatMap(b, g) => b.flatMap(g(_) flatMap f).resume
      }
    }

  final def flatMap[B](f: A => TrampolineT[M, B]): TrampolineT[M, B] =
    bind(this)(f)

  final def map[B](f: A => B)(implicit M: Applicative[M]): TrampolineT[M, B] =
    flatMap(a => Done(M.point(f(a))))
}

sealed abstract class TrampolineTInstances {

  implicit def trampolineTMonad[M[_]](implicit M: Applicative[M]): Monad[({type l[a] = TrampolineT[M, a]})#l] =
    new Monad[({type λ[α] = TrampolineT[M, α]})#λ] {
      def point[A](a: => A) =
        done(M.point(a))

      def bind[A, B](fa: TrampolineT[M, A])(f: A => TrampolineT[M, B]) =
        fa flatMap f
    }

  implicit val trampolineTHoist: Hoist[TrampolineT] =
    new Hoist[TrampolineT] {
      implicit def apply[G[_]: Monad] = trampolineTMonad[G]

      def liftM[G[_]: Monad, A](a: G[A]) = done(a)

      def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) =
        new (({type λ[α] = TrampolineT[M, α]})#λ ~> ({type λ[α] = TrampolineT[N, α]})#λ) {
          private[this] val G: Functor[({type λ[α] = M[Function0[α]]})#λ] = M.compose[Function0]
          def apply[A](a: TrampolineT[M, A]) = a.resume match {
            case \/-(b) => done(f(b))
            case -\/(b) => more(f(G.map(b)(this.apply)))
          }
        }
    }

  implicit def trampolineTEqual[M[_], A](implicit E: Equal[M[A]], M: Bind[M], T: Traverse[M]): Equal[TrampolineT[M, A]] =
    Equal.equalBy(_.run)

}
