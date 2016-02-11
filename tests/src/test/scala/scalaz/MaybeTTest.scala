package scalaz

import std.AllInstances._

object MaybeTTest extends Scalaprops {

  type MaybeTList[A] = MaybeT[List, A]
  type IntOr[A] = Int \/ A
  type MaybeTEither[A] = MaybeT[IntOr, A]

  val monadTrans = laws.monadTrans.all[MaybeT]

  val testLaws = Properties.list(
    laws.equal.all[MaybeTList[Int]],
    laws.monadPlus.all[MaybeTList],
    laws.traverse.all[MaybeTList],
    laws.monadError.all[MaybeTEither, Int]
  )

  val bindRec = laws.bindRec.laws[MaybeT[Maybe, ?]]

  object instances {
    def functor[F[_] : Functor] = Functor[MaybeT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[MaybeT[F, ?]]
    def bindRec[F[_] : Monad : BindRec] = BindRec[MaybeT[F, ?]]
    def monadError[F[_], E](implicit F: MonadError[F, E]) = MonadError[MaybeT[F, ?], E]
    def foldable[F[_] : Foldable] = Foldable[MaybeT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[MaybeT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[MaybeT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[MaybeT[F, ?]]
    def functor[F[_], E](implicit F1: MonadError[F, E], F2: Traverse[F]) = Functor[MaybeT[F, ?]]
    def apply[F[_] : Monad] = Apply[MaybeT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[MaybeT[F, ?]]
    def monadEither[E] = Monad[MaybeT[E \/ ?, ?]]
  }
}
