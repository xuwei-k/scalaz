package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object StoreTTest extends SpecLite {

  implicit def storeTuple1IntEqual = new Equal[StoreT[Tuple1, Int, Int]] {
    def equal(a1: StoreT[Tuple1, Int, Int], a2: StoreT[Tuple1, Int, Int]) = (a1.run, a2.run) match {
      case ((tf1, x1), (tf2, x2)) => Equal[Int].equal(x1, x2) && Equal[Int].equal(tf1._1(0), tf2._1(0))
    }
  }

  checkAll(comonad.laws[StoreT[Tuple1, Int, ?]])
  checkAll(applicative.laws[StoreT[Tuple1, Int, ?]])

  object instances {
    def contravarant[F[_]: Functor, I, B] = Contravariant[IndexedStoreT[F, I, ?, B]]
    def functor[F[_], A, B] = Functor[IndexedStoreT[F, ?, A, B]]
    def functor[F[_] : Functor, I, A] = Functor[IndexedStoreT[F, I, A, ?]]
    def apply[F[_] : Apply, I: Semigroup, A] = Apply[IndexedStoreT[F, I, A, ?]]
    def applicative[F[_] : Applicative, I: Monoid, A] = Applicative[IndexedStoreT[F, I, A, ?]]
    def cobind[F[_] : Cobind, A] = Cobind[StoreT[F, A, ?]]
    def comonad[F[_] : Comonad, A] = Comonad[StoreT[F, A, ?]]
    def bifunctor[F[_]: Functor, A] = Bifunctor[IndexedStoreT[F, ?, A, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Cobind, A] = Functor[StoreT[F, A, ?]]
    def functor[F[_] : Comonad, A] = Functor[StoreT[F, A, ?]]
    def functor[F[_] : Apply, I: Semigroup, A] = Functor[IndexedStoreT[F, I, A, ?]]
    def functor[F[_] : Applicative, I: Monoid, A] = Functor[IndexedStoreT[F, I, A, ?]]
    def functor[F[_] : Apply: Cobind, I: Semigroup, A] = Functor[StoreT[F, A, ?]]
    def functor[F[_] : Applicative: Cobind, I: Monoid, A] = Functor[StoreT[F, A, ?]]
    def functor[F[_] : Apply: Comonad, I: Semigroup, A] = Functor[StoreT[F, A, ?]]
    def functor[F[_] : Applicative: Comonad, I: Monoid, A] = Functor[StoreT[F, A, ?]]
    def apply[F[_] : Applicative, I: Monoid, A] = Apply[IndexedStoreT[F, I, A, ?]]
    def cobind[F[_] : Comonad, A] = Cobind[StoreT[F, A, ?]]
  }

}
