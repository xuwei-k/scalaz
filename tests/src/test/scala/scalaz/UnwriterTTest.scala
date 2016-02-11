package scalaz

import std.AllInstances._

object UnwriterTTest extends Scalaprops {

  type UnwriterTOpt[W, A] = UnwriterT[Option, W, A]
  type UnwriterTOptInt[A] = UnwriterTOpt[Int, A]

  val testLaws = Properties.list(
    laws.equal.all[UnwriterTOptInt[Int]],
    laws.bind.all[UnwriterTOptInt],
    laws.traverse.all[UnwriterTOptInt]
  )

  val bitraverse = laws.bitraverse.all[UnwriterTOpt]

  private[this] implicit def unwriterGen[A: Gen, B: Gen]: Gen[Unwriter[A, B]] =
    Gen.unwriterTGen[Id.Id, A, B]

  private[this] implicit def unwriterCogen[A: Cogen, B: Cogen]: Cogen[Unwriter[A, B]] =
    Cogen.cogenUnwriterT[Id.Id, A, B]

  val comonad = laws.comonad.all[Unwriter[Int, ?]]

  object instances {
    def equal[F[_], W, A](implicit E: Equal[F[(W, A)]]) = Equal[UnwriterT[F, W, A]]
    def functor[F[_]: Functor, W] = Functor[UnwriterT[F, W, ?]]
    def apply[F[_]: Apply, W] = Apply[UnwriterT[F, W, ?]]
    def bind[F[_]: Bind, W] = Bind[UnwriterT[F, W, ?]]
    def bifunctor[F[_]: Functor] = Bifunctor[UnwriterT[F, ?, ?]]
    def bitraverse[F[_]: Traverse] = Bitraverse[UnwriterT[F, ?, ?]]
    def foldable[F[_]: Foldable, W] = Foldable[UnwriterT[F, W, ?]]
    def traverse[F[_]: Traverse, W] = Traverse[UnwriterT[F, W, ?]]

    object Unwriter {
      def comonad[W] = Comonad[Unwriter[W, ?]]
    }
  }
}
