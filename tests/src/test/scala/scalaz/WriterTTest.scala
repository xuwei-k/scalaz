package scalaz

import std.AllInstances._
import Property.forAll

object WriterTTest extends Scalaprops {

  type WriterTOpt[W, A] = WriterT[Option, W, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]
  type IntOr[A] = Int \/ A
  type WriterTEither[A] = WriterT[IntOr, Int, A]

  val equal = laws.equal.all[WriterTOptInt[Int]]

  val monoid = laws.monoid.all[WriterTOptInt[Int]]

  val either = laws.monadError.all[WriterTEither, Int]

  val option = Properties.list(
    laws.traverse.all[WriterTOptInt],
    laws.bindRec.all[WriterTOptInt],
    laws.monadPlusStrong.all[WriterTOptInt]
  )

  val nel = laws.functor.all[WriterT[NonEmptyList, Int, ?]]

  val bitraverse = laws.bitraverse.all[WriterTOpt]

  val comonad = {
    type F[A] = Writer[Int, A]
    laws.comonad.all[F]
  }

  val `flatMapF consistent with flatMap` = forAll {
    (fa: WriterTOptInt[Int], f: Int => Option[(Int, Int)]) =>
      fa.flatMapF(f) must_=== fa.flatMap(f andThen WriterT.writerT)
  }

  private def writerTUcompilationTest: Unit = {
    import syntax.either._
    val a: String \/ (Int, Boolean) = (1, true).right[String]
    WriterT.writerTU(a)
  }

  object instances {
    def plus[F[_]: Plus, W] = Plus[WriterT[F, W, ?]]
    def plusEmpty[F[_]: PlusEmpty, W] = PlusEmpty[WriterT[F, W, ?]]
    def functor[F[_]: Functor, W] = Functor[WriterT[F, W, ?]]
    def apply[F[_]: Apply, W: Semigroup] = Apply[WriterT[F, W, ?]]
    def applicative[F[_]: Applicative, W: Monoid] = Applicative[WriterT[F, W, ?]]
    def bind[F[_]: Bind, W: Semigroup] = Bind[WriterT[F, W, ?]]
    def bindRec[F[_]: BindRec: Applicative, W: Semigroup] = BindRec[WriterT[F, W, ?]]
    def monad[F[_]: Monad, W: Monoid] = Monad[WriterT[F, W, ?]]
    def monadPlus[F[_]: MonadPlus, W: Monoid] = MonadPlus[WriterT[F, W, ?]]
    def monadError[F[_], W: Monoid, E](implicit F: MonadError[F, E]) = MonadError[WriterT[F, W, ?], E]
    def foldable[F[_]: Foldable, W] = Foldable[WriterT[F, W, ?]]
    def traverse[F[_]: Traverse, W] = Traverse[WriterT[F, W, ?]]

    // checking absence of ambiguity
    def plus[F[_]: PlusEmpty, W] = Plus[WriterT[F, W, ?]]
    def plus[F[_]: MonadPlus, W] = Plus[WriterT[F, W, ?]]
    def plusEmpty[F[_]: MonadPlus, W] = PlusEmpty[WriterT[F, W, ?]]
    def functor[F[_]: Monad, W: Monoid] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: MonadPlus, W: Monoid] = Functor[WriterT[F, W, ?]]
    def apply[F[_]: MonadPlus, W: Monoid] = Apply[WriterT[F, W, ?]]
    def apply[F[_]: Monad, W: Monoid] = Apply[WriterT[F, W, ?]]
    def apply[F[_]: Monad, W: Semigroup] = Apply[WriterT[F, W, ?]]
    def apply[F[_]: Bind, W: Monoid] = Apply[WriterT[F, W, ?]]
    def apply[F[_]: Bind, W: Semigroup] = Apply[WriterT[F, W, ?]]
    def apply[F[_]: Apply, W: Monoid] = Apply[WriterT[F, W, ?]]
    def applicative[F[_]: Monad, W: Monoid] = Applicative[WriterT[F, W, ?]]
    def applicative[F[_]: MonadPlus, W: Monoid] = Applicative[WriterT[F, W, ?]]
    def bind[F[_]: MonadPlus, W: Monoid] = Bind[WriterT[F, W, ?]]
    def bind[F[_]: Monad, W: Monoid] = Bind[WriterT[F, W, ?]]
    def bind[F[_]: Monad, W: Semigroup] = Bind[WriterT[F, W, ?]]
    def bind[F[_]: Bind, W: Monoid] = Bind[WriterT[F, W, ?]]
    def monad[F[_]: MonadPlus, W: Monoid] = Monad[WriterT[F, W, ?]]
    def functor[F[_]: Traverse, W: Monoid] = Functor[WriterT[F, W, ?]]
    def foldable[F[_]: Traverse, W] = Foldable[WriterT[F, W, ?]]
    
    object writer {
      def functor[W] = Functor[Writer[W, ?]]
      def apply[W: Semigroup] = Apply[Writer[W, ?]]
      def applicative[W: Monoid] = Applicative[Writer[W, ?]]
      def bind[W: Semigroup] = Bind[Writer[W, ?]]
      def monad[W: Monoid] = Monad[Writer[W, ?]]
      def foldable[W] = Foldable[Writer[W, ?]]
      def traverse[W] = Traverse[Writer[W, ?]]
      def comonad[W] = Comonad[Writer[W, ?]]
    }
  }
}
