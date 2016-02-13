package scalaz

import std.AllInstances._
import KleisliTest._
import CokleisliTest._

object EndomorphicTest extends Scalaprops {

  private[this] implicit def endoEqual[F[_], G[_[_], _, _], A](
    implicit F: Equal[G[F, A, A]]
  ): Equal[Endomorphic[G[F, ?, ?], A]] =
    Equal.equalBy(_.run)

  val kleisliOption = laws.monoid.all[Endomorphic[Kleisli[Option, ?, ?], Int]]
  val cokleisliOption = laws.semigroup.all[Endomorphic[Cokleisli[Option, ?, ?], Int]]

  object instances{
    def semigroup[F[_, _]: Compose, A] = Semigroup[Endomorphic[F, A]]
    def monoid[F[_, _]: Category, A] = Monoid[Endomorphic[F, A]]

    def semigroup[F[_, _]: Category, A] = Semigroup[Endomorphic[F, A]]

    object kleisli {
      def semigroup[F[_]: Bind, A] = Semigroup[Endomorphic[Kleisli[F, ?, ?], A]]
      def monoid[F[_]: Monad, A] = Monoid[Endomorphic[Kleisli[F, ?, ?], A]]

      def semigroup[F[_]: Monad, A] = Semigroup[Endomorphic[Kleisli[F, ?, ?], A]]
    }

    object cokleisli {
      def semigroup[F[_]: Cobind, A] = Semigroup[Endomorphic[Cokleisli[F, ?, ?], A]]
      def monoid[F[_]: Comonad, A] = Monoid[Endomorphic[Cokleisli[F, ?, ?], A]]

      def semigroup[F[_]: Comonad, A] = Semigroup[Endomorphic[Cokleisli[F, ?, ?], A]]
    }
  }
}
