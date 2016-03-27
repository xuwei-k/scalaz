package scalaz

import std.option._, std.anyVal._

object CoyonedaTest extends Scalaprops {

  type CoyonedaOption[A] = Coyoneda[Option, A]
  type CoyonedaNel[A] = Coyoneda[NonEmptyList, A]

  val option = Properties.list(
    laws.monadPlusStrong.all[CoyonedaOption],
    laws.bindRec.all[CoyonedaOption],
    laws.cobind.all[CoyonedaOption],
    laws.traverse.all[CoyonedaOption],
    laws.order.all[Coyoneda[Option, Int]],
    laws.foldable.all[CoyonedaOption]
  )

  val nel = Properties.list(
    laws.monad.all[CoyonedaNel],
    laws.bindRec.all[CoyonedaNel],
    laws.plus.all[CoyonedaNel],
    laws.comonad.all[CoyonedaNel],
    laws.traverse1.all[CoyonedaNel],
    laws.order.all[Coyoneda[NonEmptyList, Int]],
    laws.foldable1.all[CoyonedaNel]
  )

  object instances {
    def functor[F[_]] = Functor[Coyoneda[F, ?]]
    def contravariant[F[_]: Functor: Contravariant] = Contravariant[Coyoneda[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[Coyoneda[F, ?]]
    def foldable1[F[_]: Foldable1] = Foldable1[Coyoneda[F, ?]]
    def traverse[F[_]: Traverse] = Traverse[Coyoneda[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[Coyoneda[F, ?]]
    def apply[F[_]: Apply] = Apply[Coyoneda[F, ?]]
    def applicative[F[_]: Applicative] = Applicative[Coyoneda[F, ?]]
    def applicativePlus[F[_]: ApplicativePlus] = ApplicativePlus[Coyoneda[F, ?]]
    def bind[F[_]: Bind] = Bind[Coyoneda[F, ?]]
    def bindRec[F[_]: BindRec] = BindRec[Coyoneda[F, ?]]
    def monad[F[_]: Monad] = Monad[Coyoneda[F, ?]]
    def monadPlus[F[_]: MonadPlus] = MonadPlus[Coyoneda[F, ?]]
    def plus[F[_]: Functor: Plus] = Plus[Coyoneda[F, ?]]
    def plusEmpty[F[_]: Functor: PlusEmpty] = PlusEmpty[Coyoneda[F, ?]]
    def cobind[F[_]: Cobind] = Cobind[Coyoneda[F, ?]]
    def comonad[F[_]: Comonad] = Comonad[Coyoneda[F, ?]]
    def equal[F[_], A](implicit F: Functor[F], E: Equal[F[A]]) = Equal[Coyoneda[F, A]]
    def order[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Order[Coyoneda[F, A]]

    // checking absence of ambiguity
    def functor[F[_]: MonadPlus: Comonad: Traverse1] = Functor[Coyoneda[F, ?]]
    def foldable[F[_]: Foldable1] = Foldable[Coyoneda[F, ?]]
    def foldable[F[_]: Traverse1] = Foldable[Coyoneda[F, ?]]
    def foldable1[F[_]: Traverse1] = Foldable1[Coyoneda[F, ?]]
    def traverse[F[_]: Traverse1] = Traverse[Coyoneda[F, ?]]
    def apply[F[_]: MonadPlus] = Apply[Coyoneda[F, ?]]
    def applicative[F[_]: MonadPlus] = Applicative[Coyoneda[F, ?]]
    def applicativePlus[F[_]: MonadPlus] = ApplicativePlus[Coyoneda[F, ?]]
    def bind[F[_]: MonadPlus] = Bind[Coyoneda[F, ?]]
    def monad[F[_]: MonadPlus] = Monad[Coyoneda[F, ?]]
    def plus[F[_]: MonadPlus] = Plus[Coyoneda[F, ?]]
    def plusEmpty[F[_]: MonadPlus] = PlusEmpty[Coyoneda[F, ?]]
    def cobind[F[_]: Comonad] = Cobind[Coyoneda[F, ?]]
    def equal[F[_], A](implicit F: Functor[F], E: Order[F[A]]) = Equal[Coyoneda[F, A]]
  }

}

