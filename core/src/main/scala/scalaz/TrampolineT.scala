package scalaz

import annotation.tailrec
import std.function._
import Free.Trampoline

sealed abstract class TrampolineT[M[_], A] {
  import TrampolineT._

  final def toTrampoline(implicit M: Bind[M], T: Traverse[M]): Trampoline[M[A]] = resume(M) match {
    case \/-(a) => Trampoline.done(a.a)
    case -\/(b) =>
      val F = Bind[Trampoline]
      F.join(
        F.map(
          T.traverse(b.a)(x => Trampoline.delay(x().toTrampoline))
        )(z => F.map(T.sequence(z))(M.join))
      )
  }

  final def go(implicit M: Bind[M], T: Traverse[M]): M[A] =
    toTrampoline.run

  @tailrec
  private final def resume(implicit M: Functor[M]): More[M, A] \/ Done[M, A] = this match {
    case a @ Done(_)   => \/-(a)
    case a @ More(_)   => -\/(a)
    case a @ FlatMap() =>
      a.a match {
        case Done(b)       => -\/(More(M.map(b)(x => () => a.f(x))))
        case More(b)       => -\/(More(M.map(b)(x => Functor[Function0].map(x)(_ flatMap a.f))))
        case b @ FlatMap() => b.a.flatMap(x => b.f(x) flatMap a.f).resume
      }
  }

  final def flatMap[B](f: A => TrampolineT[M, B]): TrampolineT[M, B] =
    TrampolineT.bind(this)(f)

  final def map[B](f: A => B)(implicit M: Applicative[M]): TrampolineT[M, B] =
    trampolineTMonad[M].map(this)(f)
}

object TrampolineT {

  private final case class Done[M[_], A](a: M[A]) extends TrampolineT[M, A]
  private final case class More[M[_], A](a: M[Function0[TrampolineT[M, A]]]) extends TrampolineT[M, A]
  private sealed abstract case class FlatMap[M[_], A]() extends TrampolineT[M, A] {
    type B
    val a: TrampolineT[M, B]
    val f: B => TrampolineT[M, A]
  }

  implicit def trampolineTMonad[M[_]](implicit M: Applicative[M]): Monad[({type l[a] = TrampolineT[M, a]})#l] =
    new Monad[({type l[a] = TrampolineT[M, a]})#l] {
      def point[A](a: => A) =
        Done(M.point(a))

      def bind[A, B](fa: TrampolineT[M, A])(f: A => TrampolineT[M, B]) =
        fa flatMap f
    }

  implicit def trampolineTEqual[M[_], A](implicit E: Equal[M[A]], M: Bind[M], T: Traverse[M]): Equal[TrampolineT[M, A]] =
    Equal.equalBy(_.go)

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

  def more[M[_], A](a: M[Function0[TrampolineT[M, A]]]): TrampolineT[M, A] =
    More(a)

  def bind[M[_], A, B0](a0: TrampolineT[M, B0])(f0: B0 => TrampolineT[M, A]): TrampolineT[M, A] =
    new FlatMap[M, A] {
      type B = B0
      val a = a0
      val f = f0
    }
}

