package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.{Cogen, Arbitrary}
import Id._
import org.scalacheck.Prop.forAll

object WriterTTest extends SpecLite {

  type WriterTOpt[W, A] = WriterT[Option, W, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]
  type IntOr[A] = Int \/ A
  type WriterTEither[A] = WriterT[IntOr, Int, A]

  checkAll(equal.laws[WriterTOptInt[Int]])
  checkAll(monoid.laws[WriterTOptInt[Int]])
  checkAll(monadError.laws[WriterTEither, Int])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(bindRec.laws[WriterTOptInt])
  checkAll(monadPlus.strongLaws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
  checkAll(functor.laws[WriterT[NonEmptyList, Int, ?]])
  checkAll(bitraverse.laws[WriterTOpt])
  checkAll(monadTrans.laws[WriterT[?[_], Int, ?], List])

  implicit def writerArb[W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Writer[W, A]] =
    Applicative[Arbitrary].apply2(W, A)((w, a) => Writer[W, A](w, a))

  private[this] implicit def writerCogen[W: Cogen, A: Cogen]: Cogen[Writer[W, A]] =
    Cogen[(W, A)].contramap(_.run)

  checkAll(comonad.laws[Writer[Int, ?]])

  "flatMapF consistent with flatMap" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => Option[(Int, Int)]) =>
      fa.flatMapF(f) must_=== fa.flatMap(f andThen WriterT.writerT)
  }

  "mapF consistent with map" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => String) =>
      fa.mapF(f andThen (s => Applicative[Option].point(s))) must_=== fa.map(f)
  }

  private def writerTUcompilationTest: Unit = {
    import syntax.either._
    val a: String \/ (Int, Boolean) = (1, true).right[String]
    WriterT.writerTU(a)
  }

  object instances {
    def monoid[F[_], W, A](implicit F: Monoid[F[(W,A)]]) = Monoid[WriterT[F, W, A]]
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
    def functor[F[_]: Apply, W: Semigroup] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Apply, W: Monoid] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Bind, W: Semigroup] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Bind, W: Monoid] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Traverse, W: Semigroup] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Traverse, W: Monoid] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Monad, W: Semigroup] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: Monad, W: Monoid] = Functor[WriterT[F, W, ?]]
    def functor[F[_]: MonadPlus, W: Semigroup] = Functor[WriterT[F, W, ?]]
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
    def bindRec[F[_]: BindRec: Monad, W: Semigroup] = BindRec[WriterT[F, W, ?]]
    def bindRec[F[_]: BindRec: Applicative, W: Monoid] = BindRec[WriterT[F, W, ?]]
    def bindRec[F[_]: BindRec: Monad, W: Monoid] = BindRec[WriterT[F, W, ?]]
    def bindRec[F[_]: BindRec: MonadPlus, W: Semigroup] = BindRec[WriterT[F, W, ?]]
    def bindRec[F[_]: BindRec: MonadPlus, W: Monoid] = BindRec[WriterT[F, W, ?]]
    def monad[F[_]: MonadPlus, W: Monoid] = Monad[WriterT[F, W, ?]]
    def foldable[F[_]: Traverse, W] = Foldable[WriterT[F, W, ?]]

    object writer {
      def equal[W: Equal, A: Equal] = Equal[Writer[W, A]]
      def functor[W] = Functor[Writer[W, ?]]
      def apply[W: Semigroup] = Apply[Writer[W, ?]]
      def apply[W: Monoid] = Apply[Writer[W, ?]]
      def applicative[W: Monoid] = Applicative[Writer[W, ?]]
      def bind[W: Semigroup] = Bind[Writer[W, ?]]
      def bind[W: Monoid] = Bind[Writer[W, ?]]
      def bindRec[W: Semigroup] = BindRec[Writer[W, ?]]
      def monad[W: Monoid] = Monad[Writer[W, ?]]
      def foldable[W] = Foldable[Writer[W, ?]]
      def traverse[W] = Traverse[Writer[W, ?]]
      def comonad[W] = Comonad[Writer[W, ?]]
    }
  }
}
