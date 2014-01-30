package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

object FreeTTest extends SpecLite {

  type FreeTOptOpt[A] = FreeT[Option, Option, A]

  implicit def freeArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[Free[F, A]] =
  Arbitrary(Gen.frequency(
    (1, Functor[Arbitrary].map(A)(Free.Return[F, A](_)).arbitrary),
    (1, Functor[Arbitrary].map(F(freeArb[F, A]))(Free.Suspend[F, A](_)).arbitrary)
  ))

  trait Template[G[_], F[_]] extends (G ~> ({type λ[α] = G[F[α]]})#λ) {
    override final def apply[A](a: G[A]) = lift(a)

    def lift[A: G]: G[F[A]]
  }

  implicit val optEq = new Template[Equal, Option]{
    def lift[A: Equal] = implicitly
  }
    
  implicit val optArb = new Template[Arbitrary, Option]{
    def lift[A: Arbitrary] = implicitly
  }

  implicit def freeTArb[F[_], M[_], A](implicit A: Arbitrary[M[Free[F, A]]], F: Functor[F], M: Monad[M]): Arbitrary[FreeT[F, M, A]] =
    Functor[Arbitrary].map(A)(FreeT.fromFree[F, M, A](_))

  {
    implicit def freeTOptOptEq: Equal[FreeT[Option, Option, Int]] = Equal.equal((a,b) => a.run == b.run)

    checkAll(monad.laws[FreeTOptOpt])
  }
}

