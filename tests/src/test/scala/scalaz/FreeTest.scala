package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import Free.{Return, Suspend}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._

object FreeTest extends SpecLite {

  implicit def freeArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[Free[F, A]] = {
    def loop(pure: Int, suspend: Int): Arbitrary[Free[F, A]] =
      Arbitrary(Gen.frequency(
        (pure, Functor[Arbitrary].map(A)(Return[F, A](_)).arbitrary),
        (suspend, Functor[Arbitrary].map(F(loop(pure + 1, suspend)))(Suspend[F, A](_)).arbitrary)
      ))
    loop(1, 1)
  }

  private trait Template[F[_], G[_]] extends (G ~> ({type λ[α] = G[F[α]]})#λ) {
    override final def apply[A](a: G[A]) = lift(a)

    def lift[A: G]: G[F[A]]
  }

  "OneAnd[Option, A]" should {
    type OneAndOpt[A] = OneAnd[Option, A]
    type FreeOneAndOpt[A] = Free[OneAndOpt, A]

    implicit val oneAndOptArb = new Template[OneAndOpt, Arbitrary] {
      def lift[A: Arbitrary] = implicitly[Arbitrary[OneAndOpt[A]]]
    }

    implicit val oneAndOptEqual = new Template[OneAndOpt, Equal] {
      def lift[A: Equal] = Equal[OneAndOpt[A]]
    }

    checkAll(traverse1.laws[FreeOneAndOpt])
    checkAll(monad.laws[FreeOneAndOpt])
    checkAll(equal.laws[FreeOneAndOpt[Int]])
  }

  "List" should {
    type FreeList[A] = Free[List, A]

    implicit val listArb = new Template[List, Arbitrary] {
      def lift[A](implicit A: Arbitrary[A]) = Arbitrary(
        Gen.choose(0, 2).flatMap(Gen.listOfN(_, A.arbitrary))
      )
    }

    implicit val listEq = new Template[List, Equal] {
      def lift[A: Equal] = Equal[List[A]]
    }

    checkAll(traverse.laws[FreeList])
    checkAll(monadPlus.laws[FreeList])
    checkAll(equal.laws[FreeList[Int]])
  }

}

