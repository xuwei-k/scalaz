package scalaz

import scala.annotation.tailrec
import Id._

sealed abstract class IndexedStateT[F[_], -S1, S2, A] { self =>
  import IndexedStateT._

  /** Run and return the final value and state in the context of `F` */
  @tailrec
  final def apply(initial: S1)(implicit F: Bind[F]): F[(S2, A)] =
    this match {
      case Wrap(f) => f(initial)
      case FlatMap(Wrap(f), g) => F.bind(f(initial)) { case (sx, x) => g(sx, x).run(sx) }
      case FlatMap(FlatMap(f, g), h) => f.flatMapS((sx, x) => g(sx, x).flatMapS(h)).apply(initial)
    }

  /** An alias for `apply` */
  def run(initial: S1)(implicit F: Bind[F]): F[(S2, A)] = apply(initial)

  /** Run and return the final value and state in the context of `F` */
  def runRec(initial: S1)(implicit F: BindRec[F]): F[(S2, A)] = {
    abstract class Eval {
      type S0
      val s0: S0
      val st: IndexedStateT[F, S0, S2, A]

      @tailrec
      final def step: F[Eval \/ (S2, A)] = st match {
        case Wrap(f) => F.map(f(s0))(\/.right)
        case FlatMap(Wrap(f), g) => F.map(f(s0)){ case (sx, x) => \/.left(Eval(g(sx, x), sx)) }
        case FlatMap(FlatMap(f, g), h) => Eval(f.flatMapS((sx, x) => g(sx, x).flatMapS(h)), s0).step
      }
    }

    object Eval {
      def apply[S](f: IndexedStateT[F, S, S2, A], s: S): Eval = new Eval {
        type S0 = S
        val s0 = s
        val st = f
      }
    }

    F.tailrecM((e: Eval) => e.step)(Eval(this, initial))
  }

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZero[S <: S1](implicit S: Monoid[S], F: Bind[F]): F[(S2, A)] =
    run(S.zero)

  /** Calls `run` using `Monoid[S].zero` as the initial state */
  def runZeroRec[S <: S1](implicit S: Monoid[S], F: BindRec[F]): F[(S2, A)] =
    runRec(S.zero)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def eval(initial: S1)(implicit F: Bind[F]): F[A] =
    F.map(run(initial))(_._2)

  /** Run, discard the final state, and return the final value in the context of `F` */
  def evalRec(initial: S1)(implicit F: BindRec[F]): F[A] =
    F.map(runRec(initial))(_._2)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZero[S <: S1](implicit F: Bind[F], S: Monoid[S]): F[A] =
    eval(S.zero)

  /** Calls `eval` using `Monoid[S].zero` as the initial state */
  def evalZeroRec[S <: S1](implicit F: BindRec[F], S: Monoid[S]): F[A] =
    evalRec(S.zero)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def exec(initial: S1)(implicit F: Bind[F]): F[S2] =
    F.map(run(initial))(_._1)

  /** Run, discard the final value, and return the final state in the context of `F` */
  def execRec(initial: S1)(implicit F: BindRec[F]): F[S2] =
    F.map(runRec(initial))(_._1)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZero[S <: S1](implicit F: Bind[F], S: Monoid[S]): F[S2] =
    exec(S.zero)

  /** Calls `exec` using `Monoid[S].zero` as the initial state */
  def execZeroRec[S <: S1](implicit F: BindRec[F], S: Monoid[S]): F[S2] =
    execRec(S.zero)

  def map[B](f: A => B)(implicit F: Applicative[F]): IndexedStateT[F, S1, S2, B] =
    flatMap(a => StateT[F, S2, B](s => F.point((s, f(a)))))

  def xmap[X1, X2](f: S2 => X1)(g: X2 => S1)(implicit F: Applicative[F]): IndexedStateT[F, X2, X1, A] =
    imap(f).contramap(g)

  /** Map both the return value and final state using the given function. */
  def mapK[G[_], B, S](f: F[(S2, A)] => G[(S, B)])(implicit M: Bind[F]): IndexedStateT[G, S1, S, B] =
    IndexedStateT(s => f(apply(s)))

  import BijectionT._
  def bmap[X, S >: S2 <: S1](b: Bijection[S, X])(implicit F: Applicative[F]): StateT[F, X, A] =
    xmap(b to _)(b from _)

  def contramap[X](g: X => S1)(implicit F: Applicative[F]): IndexedStateT[F, X, S2, A] =
    IndexedStateT((s: X) => F.point((g(s), ()))).flatMap(_ => this)

  def imap[X](f: S2 => X)(implicit F: Applicative[F]): IndexedStateT[F, S1, X, A] =
    flatMap(a => IndexedStateT(s2 => F.point((f(s2), a))))

  def bimap[X, B](f: S2 => X)(g: A => B)(implicit F: Applicative[F]): IndexedStateT[F, S1, X, B] =
    flatMap(a => IndexedStateT(s2 => F.point((f(s2), g(a)))))

  def leftMap[X](f: S2 => X)(implicit F: Applicative[F]): IndexedStateT[F, S1, X, A] =
    imap(f)

  def flatMap[S3, B](f: A => IndexedStateT[F, S2, S3, B]): IndexedStateT[F, S1, S3, B] =
    flatMapS((s2, a) => f(a))

  def lift[M[_]](implicit F: Bind[F], M: Applicative[M]): IndexedStateT[λ[α => M[F[α]]], S1, S2, A] =
    IndexedStateT[λ[α => M[F[α]]], S1, S2, A](s => M.point(self(s)))

  import Liskov._
  def unlift[M[_], FF[_], S <: S1](implicit M: Comonad[M], F: Bind[λ[α => M[FF[α]]]], ev: this.type <~< IndexedStateT[λ[α => M[FF[α]]], S, S2, A]): IndexedStateT[FF, S, S2, A] =
    IndexedStateT(s => M.copoint(ev(self)(s)))

  def unliftId[M[_], S <: S1](implicit M: Comonad[M], F: Bind[M], ev: this.type <~< IndexedStateT[M, S, S2, A]): IndexedState[S, S2, A] = unlift[M, Id, S]

  def rwst[W, R](implicit F: Bind[F], W: Monoid[W]): IndexedReaderWriterStateT[F, R, W, S1, S2, A] =
    IndexedReaderWriterStateT(
      (r, s) => F.map(run(s)) {
        case (s, a) => (W.zero, a, s)
      }
    )

  def zoom[S0, S3, S <: S1](l: LensFamily[S0, S3, S, S2])(implicit F: Applicative[F]): IndexedStateT[F, S0, S3, A] =
    IndexedStateT((s0: S0) => F.point((s0, ()))).flatMapS((s0, _) =>
      this.contramap(l get (_: S0)).imap[S3](l.set(s0, _))
    )

  def liftF[S <: S1](implicit F: Functor[IndexedStateT[F, S, S2, ?]]) =
    Free.liftF[IndexedStateT[F, S, S2, ?], A](self)

  private def flatMapS[S3, B](f: (S2, A) => IndexedStateT[F, S2, S3, B]): IndexedStateT[F, S1, S3, B] =
    FlatMap(this, f)
}

object IndexedStateT extends StateTInstances with StateTFunctions {
  private final case class Wrap[F[_], S1, S2, A](run: S1 => F[(S2, A)]) extends IndexedStateT[F, S1, S2, A]
  private final case class FlatMap[F[_], S1, S2, S3, A, B](a: IndexedStateT[F, S1, S2, A], f: (S2, A) => IndexedStateT[F, S2, S3, B]) extends IndexedStateT[F, S1, S3, B]

  def apply[F[_], S1, S2, A](f: S1 => F[(S2, A)]): IndexedStateT[F, S1, S2, A] =
    Wrap(f)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class IndexedStateTInstances2 {
  implicit def indexedStateTContravariant[S2, A0, F[_]](implicit F0: Applicative[F]): Contravariant[IndexedStateT[F, ?, S2, A0]] =
    new IndexedStateTContravariant[S2, A0, F] {
      implicit def F = F0
    }
}

sealed abstract class IndexedStateTInstances1 extends IndexedStateTInstances2 {
  implicit def indexedStateTFunctorLeft[S1, A0, F[_]](implicit F0: Applicative[F]): Functor[IndexedStateT[F, S1, ?, A0]] =
    new IndexedStateTFunctorLeft[S1, A0, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class IndexedStateTInstances0 extends IndexedStateTInstances1 {
  implicit def indexedStateTBifunctor[S1, F[_]](implicit F0: Applicative[F]): Bifunctor[IndexedStateT[F, S1, ?, ?]] =
    new IndexedStateTBifunctor[S1, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class IndexedStateTInstances extends IndexedStateTInstances0 {
  implicit def indexedStateTFunctorRight[S1, S2, F[_]](implicit F0: Applicative[F]): Functor[IndexedStateT[F, S1, S2, ?]] =
    new IndexedStateTFunctorRight[S1, S2, F] {
      implicit def F: Applicative[F] = F0
    }

  implicit def indexedStateTPlus[F[_]: Bind: Plus, S1, S2]: Plus[IndexedStateT[F, S1, S2, ?]] =
    new IndexedStateTPlus[F, S1, S2] {
      def F = implicitly
      def G = implicitly
    }
}

sealed abstract class StateTInstances3 extends IndexedStateTInstances {
  implicit def stateTBindRec[S, F[_]](implicit F0: Applicative[F]): BindRec[StateT[F, S, ?]] =
    new StateTBindRec[S, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class StateTInstances2 extends StateTInstances3 {
  implicit def stateTMonadState[S, F[_]](implicit F0: Applicative[F]): MonadState[StateT[F, S, ?], S] =
    new StateTMonadState[S, F] {
      implicit def F: Applicative[F] = F0
    }
}

sealed abstract class StateTInstances1 extends StateTInstances2 {
  implicit def stateTMonadPlus[S, F[_]](implicit F0: MonadPlus[F]): MonadPlus[StateT[F, S, ?]] =
    new StateTMonadStateMonadPlus[S, F] {
      implicit def F: MonadPlus[F] = F0
    }
}

sealed abstract class StateTInstances0 extends StateTInstances1 {
  implicit def StateMonadTrans[S]: Hoist[λ[(g[_], a) => StateT[g, S, a]]] =
    new StateTHoist[S] {}
}

abstract class StateTInstances extends StateTInstances0 {
  implicit def stateMonad[S]: MonadState[State[S, ?], S] =
      StateT.stateTMonadState[S, Id](Id.id)
}

trait IndexedStateTFunctions {
  def constantIndexedStateT[F[_], S1, S2, A](a: A)(s: => S2)(implicit F: Applicative[F]): IndexedStateT[F, S1, S2, A] =
    IndexedStateT((_: S1) => F.point((s, a)))
}

trait StateTFunctions extends IndexedStateTFunctions {
  def constantStateT[F[_], S, A](a: A)(s: => S)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT((_: S) => F.point((s, a)))

  def stateT[F[_], S, A](a: A)(implicit F: Applicative[F]): StateT[F, S, A] =
    StateT(s => F.point((s, a)))
}

//
// Implementation traits for type class instances
//

private trait IndexedStateTContravariant[S2, A0, F[_]] extends Contravariant[IndexedStateT[F, ?, S2, A0]] {
  implicit def F: Applicative[F]

  override def contramap[A, B](fa: IndexedStateT[F, A, S2, A0])(f: B => A): IndexedStateT[F, B, S2, A0] = fa.contramap(f)
}

private trait IndexedStateTBifunctor[S1, F[_]] extends Bifunctor[IndexedStateT[F, S1, ?, ?]] {
  implicit def F: Applicative[F]

  override def bimap[A, B, C, D](fab: IndexedStateT[F, S1, A, B])(f: A => C, g: B => D): IndexedStateT[F, S1, C, D] = fab.bimap(f)(g)
}

private trait IndexedStateTFunctorLeft[S1, A0, F[_]] extends Functor[IndexedStateT[F, S1, ?, A0]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: IndexedStateT[F, S1, A, A0])(f: A => B): IndexedStateT[F, S1, B, A0] = fa.imap(f)
}

private trait IndexedStateTFunctorRight[S1, S2, F[_]] extends Functor[IndexedStateT[F, S1, S2, ?]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: IndexedStateT[F, S1, S2, A])(f: A => B): IndexedStateT[F, S1, S2, B] = fa.map(f)
}

private trait StateTBind[S, F[_]] extends Bind[StateT[F, S, ?]] {
  implicit def F: Applicative[F]

  override def map[A, B](fa: StateT[F, S, A])(f: A => B): StateT[F, S, B] = fa.map(f)

  def bind[A, B](fa: StateT[F, S, A])(f: A => StateT[F, S, B]): StateT[F, S, B] = fa.flatMap(f)
}

private trait StateTBindRec[S, F[_]] extends StateTBind[S, F] with BindRec[StateT[F, S, ?]] {
  def tailrecM[A, B](f: A => StateT[F, S, A \/ B])(a: A): StateT[F, S, B] = {
    f(a).flatMap(_ match {
      case -\/(a) => tailrecM(f)(a)
      case \/-(b) => StateT(s => F.point((s, b)))
    })
  }
}

private trait StateTMonadState[S, F[_]] extends MonadState[StateT[F, S, ?], S] with StateTBind[S, F] {
  implicit def F: Applicative[F]

  def point[A](a: => A): StateT[F, S, A] = {
    val aa = Need(a)
    StateT(s => F.point(s, aa.value))
  }

  def init: StateT[F, S, S] = StateT(s => F.point((s, s)))

  def get = init

  def put(s: S): StateT[F, S, Unit] = StateT(_ => F.point((s, ())))

  override def modify(f: S => S): StateT[F, S, Unit] = StateT(s => F.point((f(s), ())))

  override def gets[A](f: S => A): StateT[F, S, A] = StateT(s => F.point((s, f(s))))
}

private trait StateTHoist[S] extends Hoist[StateT[?[_], S, ?]] {

  def liftM[G[_], A](ga: G[A])(implicit G: Monad[G]): StateT[G, S, A] =
    StateT(s => G.map(ga)(a => (s, a)))

  def hoist[M[_]: Monad, N[_]](f: M ~> N) = new (StateT[M, S, ?] ~> StateT[N, S, ?]) {
    def apply[A](action: StateT[M, S, A]) = IndexedStateT(s => f(action.run(s)))
  }

  implicit def apply[G[_] : Monad]: Monad[StateT[G, S, ?]] = StateT.stateTMonadState[S, G]
}

private trait IndexedStateTPlus[F[_], S1, S2] extends Plus[IndexedStateT[F, S1, S2, ?]] {
  implicit def F: Bind[F]
  implicit def G: Plus[F]
  override final def plus[A](a: IndexedStateT[F, S1, S2, A], b: => IndexedStateT[F, S1, S2, A]) =
    IndexedStateT(s => G.plus(a.run(s), b.run(s)))
}

private trait StateTMonadStateMonadPlus[S, F[_]] extends StateTMonadState[S, F] with StateTHoist[S] with MonadPlus[StateT[F, S, ?]] with IndexedStateTPlus[F, S, S] {
  implicit def F: MonadPlus[F]
  override final def G = F

  def empty[A]: StateT[F, S, A] = liftM[F, A](F.empty[A])
}
