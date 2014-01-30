package scalaz

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

object FreeTTest extends SpecLite {

  implicit def freeEqual[F[_], A](implicit A: Equal[A], N: Equal ~> ({type λ[α] = Equal[F[α]]})#λ, F: Functor[F]): Equal[Free[F, A]] =
    Equal.equal{ (aa, bb) =>
      (aa.resume, bb.resume) match {
        case (\/-(a), \/-(b)) => A.equal(a, b)
        case (-\/(a), -\/(b)) => N(freeEqual[F, A]).equal(a, b)
        case (\/-(_), -\/(_)) => false
        case (-\/(_), \/-(_)) => false
      }
    }

  implicit def freeArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[Free[F, A]] =
  Arbitrary(Gen.frequency(
    (1, Functor[Arbitrary].map(A)(Free.Return[F, A](_)).arbitrary),
    (1, Functor[Arbitrary].map(F(freeArb[F, A]))(Free.Suspend[F, A](_)).arbitrary)
  ))

  implicit def freeTArb[F[_], M[_], A](implicit A: Arbitrary[M[Free[F, A]]], F: Functor[F], M: Monad[M]): Arbitrary[FreeT[F, M, A]] =
    Functor[Arbitrary].map(A)(FreeT.fromFree[F, M, A](_))

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

  implicit val listEq = new Template[Equal, List]{
    def lift[A: Equal] = implicitly
  }

  implicit val listArb = new Template[Arbitrary, List]{
    def lift[A](implicit A: Arbitrary[A]) = Arbitrary(
      Gen.choose(0, 3).flatMap(Gen.listOfN(_, A.arbitrary)) // avoid stack overflow
    )
  }

  type FreeTOptOpt[A]   = FreeT[Option, Option, A]
  type FreeTOptList[A]  = FreeT[Option, List, A]
  type FreeTListOpt[A]  = FreeT[List, Option, A]
  type FreeTListList[A]  = FreeT[List, List, A]

  checkAll("FreeT[Option, Option, _]", monad.laws[FreeTOptOpt])
  checkAll("FreeT[Option, List, _]"  , monad.laws[FreeTOptList])
  checkAll("FreeT[List, Option, _]"  , monad.laws[FreeTListOpt])
  checkAll("FreeT[List, List, _]"    , monad.laws[FreeTListList])

  property("FreeT[F, Id, A] is Free[F, A]") = forAll{ a: Free[List, Int] =>
    implicit val s = Show.showA[Free[List, Int]]
    FreeT.fromFree[List, Id.Id, Int](a).toFree must_=== a
  }
}

