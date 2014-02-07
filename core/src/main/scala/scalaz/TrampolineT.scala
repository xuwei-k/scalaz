package scalaz

import annotation.tailrec
import std.function._
import Free.Trampoline
import TrampolineT._

object TrampolineT extends TrampolineTInstances {

  private final case class Done[F[_], A](a: F[A]) extends TrampolineT[F, A]
  private final case class More[F[_], A](a: F[Function0[TrampolineT[F, A]]]) extends TrampolineT[F, A]
  private final case class FlatMap[F[_], A, B](a: TrampolineT[F, B], f: B => TrampolineT[F, A]) extends TrampolineT[F, A]

  def from[F[_], A](m: F[Trampoline[A]])(implicit F: Applicative[F]): TrampolineT[F, A] = {
    def loop(f: Trampoline[A]): TrampolineT[F, A] = f.resume match{
      case -\/(s) =>
        More(F.point(Functor[Function0].map(s)(loop)))
      case \/-(r) =>
        Done(F.point(r))
    }

    More(F.map(m)(x => () => loop(x)))
  }

  def done[F[_], A](a: F[A]): TrampolineT[F, A] =
    Done(a)

  def delay[F[_], A](a: => F[A])(implicit F: Applicative[F]): TrampolineT[F, A] =
    More(F.point(() => Done(a)))

  def more[F[_], A](a: F[Function0[TrampolineT[F, A]]]): TrampolineT[F, A] =
    More(a)

  def suspend[F[_], A](a: => TrampolineT[F, A])(implicit F: Applicative[F]): TrampolineT[F, A] =
    More(F.point(() => a))

  def bind[F[_], A, B](a: TrampolineT[F, B])(f: B => TrampolineT[F, A]): TrampolineT[F, A] =
    FlatMap(a, f)
}

/** Trampoline Monad Transformer */
sealed abstract class TrampolineT[F[_], A] {

  final def toTrampoline(implicit F: Bind[F], T: Traverse[F]): Trampoline[F[A]] =
    resume(F) match {
      case \/-(a) => Trampoline.done(a)
      case -\/(a) =>
        val G = Bind[Trampoline]
        G.join(
          G.map(
            T.traverse(a)(x => Trampoline.delay(x().toTrampoline))
          )(z => G.map(T.sequence(z))(F.join))
        )
    }

  final def run(implicit F: Bind[F], T: Traverse[F]): F[A] =
    toTrampoline.run

  @tailrec
  final def resume(implicit F: Functor[F]): F[Function0[TrampolineT[F, A]]] \/ F[A] =
    this match {
      case Done(a)       => \/-(a)
      case More(a)       => -\/(a)
      case FlatMap(a, f) => a match {
        case Done(b)       => -\/(F.map(b)(x => () => f(x)))
        case More(b)       => -\/(F.map(b)(x => Functor[Function0].map(x)(_ flatMap f)))
        case FlatMap(b, g) => b.flatMap(g(_) flatMap f).resume
      }
    }

  final def flatMap[B](f: A => TrampolineT[F, B]): TrampolineT[F, B] =
    bind(this)(f)

  final def map[B](f: A => B)(implicit F: Applicative[F]): TrampolineT[F, B] =
    flatMap(a => Done(F.point(f(a))))
}

sealed abstract class TrampolineTInstances {

  implicit def trampolineTMonad[F[_]](implicit F: Applicative[F]): Monad[({type l[a] = TrampolineT[F, a]})#l] =
    new Monad[({type λ[α] = TrampolineT[F, α]})#λ] {
      def point[A](a: => A) =
        done(F.point(a))

      def bind[A, B](fa: TrampolineT[F, A])(f: A => TrampolineT[F, B]) =
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

  implicit def trampolineTEqual[F[_], A](implicit E: Equal[F[A]], F: Bind[F], T: Traverse[F]): Equal[TrampolineT[F, A]] =
    Equal.equalBy(_.run)

}
