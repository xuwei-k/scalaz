package scalaz

import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._
import KleisliTest._

object EndomorphicTest extends SpecLite {

  checkAll(invariantFunctor.laws[({type l[a] = Endomorphic[({type λ[α, β] = Kleisli[Option, α, β]})#λ, a]})#l])
  checkAll(monoid.laws[Endomorphic[({type λ[α, β] = Kleisli[Option, α, β]})#λ, Int]])

  object instances{
    def invariantFunctor[F[_, _]: Profunctor] = InvariantFunctor[({type λ[α] = Endomorphic[F, α]})#λ]
    def semigroup[F[_, _]: Compose, A] = Semigroup[Endomorphic[F, A]]
    def monoid[F[_, _]: Category, A] = Monoid[Endomorphic[F, A]]

    def semigroup[F[_, _]: Category, A] = Semigroup[Endomorphic[F, A]]

    object kleisli {
      def invariantFunctor[F[_]: Functor] = InvariantFunctor[({type l[a] = Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, a]})#l]
      def semigroup[F[_]: Bind, A] = Semigroup[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]
      def monoid[F[_]: Monad, A] = Monoid[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]

      def semigroup[F[_]: Monad, A] = Semigroup[Endomorphic[({type λ[α, β] = Kleisli[F, α, β]})#λ, A]]
    }

    object cokleisli {
      def invariantFunctor[F[_]: Functor] = InvariantFunctor[({type l[a] = Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, a]})#l]
      def semigroup[F[_]: Cobind, A] = Semigroup[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]
      def monoid[F[_]: Comonad, A] = Monoid[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]

      def semigroup[F[_]: Comonad, A] = Semigroup[Endomorphic[({type λ[α, β] = Cokleisli[F, α, β]})#λ, A]]
    }
  }
}
