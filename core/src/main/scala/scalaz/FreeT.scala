package scalaz

import FreeT._

object FreeT extends FreeTInstances {
  /** Suspend the computation with the given suspension. */
  private case class Suspend[S[_], M[_], A](a: M[A \/ Coyoneda[S, FreeT[S, M, A]]]) extends FreeT[S, M, A]

  /** Call a subroutine and continue with the given function. */
  private case class Gosub[S[_], M[_], B, C](a: FreeT[S, M, C], f: C => FreeT[S, M, B]) extends FreeT[S, M, B]

  /** Return the given value in the free monad. */
  def point[S[_], M[_], A](value: A)(implicit M: Applicative[M]): FreeT[S, M, A] = Suspend(M.point(-\/(value)))

  def suspend[S[_], M[_], A](a: M[A \/ S[FreeT[S, M, A]]])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(a)(_.map(Coyoneda.lift)))

  def tailrecM[S[_], M[_]: Applicative, A, B](f: A => FreeT[S, M, A \/ B])(a: A): FreeT[S, M, B] =
    f(a).flatMap {
      case -\/(a0) => tailrecM(f)(a0)
      case \/-(b) => point[S, M, B](b)
    }

  def liftM[S[_], M[_], A](value: M[A])(implicit M: Functor[M]): FreeT[S, M, A] =
    Suspend(M.map(value)(\/.left))

  /** Suspends a value within a functor in a single step. Monadic unit for a higher-order monad. */
  def liftF[S[_], M[_], A](value: S[A])(implicit M: Applicative[M]): FreeT[S, M, A] =
    Suspend(M.point(\/-(Coyoneda.lift(value).map(point[S, M, A]))))

  def roll[S[_], M[_], A](value: S[FreeT[S, M, A]])(implicit M: Applicative[M]): FreeT[S, M, A] =
    liftF[S, M, FreeT[S, M, A]](value).flatMap(identity)
}

sealed abstract class FreeT[S[_], M[_], A] {
  final def map[B](f: A => B)(implicit M: Functor[M]): FreeT[S, M, B] =
    this match {
      case Suspend(m) => Suspend(M.map(m)(_.bimap(f, Functor[Coyoneda[S, ?]].lift(_.map(f)))))
      case Gosub(a, k) => Gosub(a, (x: Any) => k(x).map(f))
    }

  /** Binds the given continuation to the result of this computation. */
  final def flatMap[B](f: A => FreeT[S, M, B]): FreeT[S, M, B] =
    this match {
      case Gosub(a, k) => Gosub(a, (x: Any) => Gosub(k(x), f))
      case a => Gosub(a, f)
    }

  /** Evaluates a single layer of the free monad **/
  def resume(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A \/ S[FreeT[S, M, A]]] = {
    def go(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ (A \/ S[FreeT[S, M, A]])] =
      ft match {
        case Suspend(f) => M0.map(f)(a => \/-(a.map(_.run)))
        case Gosub(m0, f0) => m0 match {
          case Suspend(m1) => M0.map(m1) {
            case -\/(a) => -\/(f0(a))
            case \/-(fc) => \/-(\/-(Functor[Coyoneda[S, ?]].map(fc)(_.flatMap(f0)).run))
          }
          case Gosub(m1, f1) => M1.point(-\/(m1.flatMap(f1(_).flatMap(f0))))
        }
      }

    M0.tailrecM(go)(this)
  }

  /**
    * Runs to completion, using a function that maps the resumption from `S` to a monad `M`.
    */
  def runM(interp: S[FreeT[S, M, A]] => M[FreeT[S, M, A]])(implicit S: Functor[S], M0: BindRec[M], M1: Applicative[M]): M[A] = {
    def runM2(ft: FreeT[S, M, A]): M[FreeT[S, M, A] \/ A] =
      M0.bind(ft.resume) {
        case -\/(a) => M1.point(\/-(a))
        case \/-(fc) => M0.map(interp(fc))(\/.left)
      }

    M0.tailrecM(runM2)(this)
  }
}

sealed abstract class FreeTInstances2 {
  implicit def freeTBind[S[_], M[_]](implicit M0: Functor[M]): Bind[FreeT[S, M, ?]] =
    new FreeTBind[S, M] {
      implicit def M: Functor[M] = M0
    }

  implicit def freeTMonadTrans[S[_]]: MonadTrans[FreeT[S, ?[_], ?]] =
    new MonadTrans[FreeT[S, ?[_], ?]] {
      def liftM[G[_]: Monad, A](a: G[A]) =
        FreeT.liftM(a)
      def apply[G[_]: Monad] =
        Monad[FreeT[S, G, ?]]
    }

  implicit def freeTFoldable[S[_]: Foldable: Functor, M[_]: Foldable: Applicative: BindRec]: Foldable[FreeT[S, M, ?]] =
    new FreeTFoldable[S, M] {
      override def S = implicitly
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances1 extends FreeTInstances2 {
  implicit def freeTTraverse[S[_]: Traverse, M[_]: Traverse: Applicative: BindRec]: Traverse[FreeT[S, M, ?]] =
    new FreeTTraverse[S, M] {
      override def F = implicitly
      override def M = implicitly
      override def M1 = implicitly
      override def M2 = implicitly
    }
}

sealed abstract class FreeTInstances0 extends FreeTInstances1 {
  implicit def freeTMonad[S[_], M[_]](implicit M0: Applicative[M]): Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] =
    new FreeTMonad[S, M] {
      def M = M0
    }

  implicit def freeTPlus[S[_], M[_]](implicit S0: Functor[S], F0: Applicative[M], M0: BindRec[M], G: Plus[M]): Plus[FreeT[S, M, ?]] =
    new FreeTPlus[S, M] {
      override def S = S0
      override def M = F0
      override def M1 = M0
      override def M2 = G
    }
}

sealed abstract class FreeTInstances extends FreeTInstances0 {
  implicit def freeTMonadPlus[S[_], M[_]](implicit S0: Functor[S], F0: ApplicativePlus[M], M0: BindRec[M]): MonadPlus[FreeT[S, M, ?]] =
    new MonadPlus[FreeT[S, M, ?]] with FreeTPlus[S, M] with FreeTMonad[S, M] {
      override def S = S0
      override def M1 = M0
      override def M2 = F0
      override def M = F0

      override def empty[A] = FreeT.liftM[S, M, A](F0.empty[A])(M)
    }
}

private trait FreeTBind[S[_], M[_]] extends Bind[FreeT[S, M, ?]] {
  implicit def M: Functor[M]

  override final def map[A, B](fa: FreeT[S, M, A])(f: A => B): FreeT[S, M, B] = fa.map(f)
  def bind[A, B](fa: FreeT[S, M, A])(f: A => FreeT[S, M, B]): FreeT[S, M, B] = fa.flatMap(f)
}

private trait FreeTMonad[S[_], M[_]] extends Monad[FreeT[S, M, ?]] with BindRec[FreeT[S, M, ?]] with FreeTBind[S, M] {
  implicit def M: Applicative[M]

  override final def point[A](a: => A) =
    FreeT.point[S, M, A](a)
  override final def tailrecM[A, B](f: A => FreeT[S, M, A \/ B])(a: A) =
    FreeT.tailrecM(f)(a)
}

private trait FreeTPlus[S[_], M[_]] extends Plus[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Applicative[M]
  implicit def M1: BindRec[M]
  def M2: Plus[M]
  override final def plus[A](a: FreeT[S, M, A], b: => FreeT[S, M, A]) =
    FreeT.suspend(M2.plus(a.resume, b.resume))(M)
}

private trait FreeTFoldable[S[_], M[_]] extends Foldable[FreeT[S, M, ?]] with Foldable.FromFoldMap[FreeT[S, M, ?]] {
  implicit def S: Functor[S]
  implicit def M: Applicative[M]
  implicit def M1: BindRec[M]
  def F: Foldable[S]
  def M2: Foldable[M]

  override final def foldMap[A, B: Monoid](fa: FreeT[S, M, A])(f: A => B): B =
    M2.foldMap(fa.resume){
      case \/-(a) =>
        F.foldMap(a)(foldMap(_)(f))
      case -\/(a) =>
        f(a)
    }
}

private trait FreeTTraverse[S[_], M[_]] extends Traverse[FreeT[S, M, ?]] with FreeTFoldable[S, M] with FreeTBind[S, M] {
  override final def S: Functor[S] = F
  override implicit def F: Traverse[S]
  override def M2: Traverse[M]
  override implicit def M: Applicative[M]
  override implicit def M1: BindRec[M]

  override final def traverseImpl[G[_], A, B](fa: FreeT[S, M, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(
      M2.traverseImpl(fa.resume){
        case \/-(a) =>
          G.map(F.traverseImpl(a)(traverseImpl(_)(f)))(FreeT.roll(_))
        case -\/(a) =>
          G.map(f(a))(FreeT.point[S, M, B])
      }
    )(FreeT.liftM(_)(M).flatMap(identity))
}
