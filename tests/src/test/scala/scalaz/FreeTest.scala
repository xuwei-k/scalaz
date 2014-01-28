package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import Free._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._

object FreeTest1 extends SpecLite {

  type PairOpt[A] = Option[(A, A)]
  type BinaryTree[A] = Free[PairOpt, A]

  implicit val pairOptInstance: Traverse[PairOpt] =
    new Traverse[PairOpt] {
      override def map[A, B](fa: PairOpt[A])(f: A => B) =
        fa.map{case (a, b) => (f(a), f(b))}
      def traverseImpl[G[_], A, B](fa: PairOpt[A])(f: A => G[B])(implicit G: Applicative[G]) =
        Traverse[Option].traverse(fa){case (a, b) => G.tuple2(f(a), f(b))}
    }

  implicit val pairOptArb: Arbitrary ~> ({type λ[α] = Arbitrary[PairOpt[α]]})#λ =
    new (Arbitrary ~> ({type λ[α] = Arbitrary[PairOpt[α]]})#λ){
      def apply[A](a: Arbitrary[A]) = {
        implicit val a0 = a
        implicitly[Arbitrary[PairOpt[A]]]
      }
    }

  implicit def freeArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[Free[F, A]] =
    Arbitrary(Gen.oneOf(
      Functor[Arbitrary].map(A)(Return[F, A](_)).arbitrary,
      Functor[Arbitrary].map(F(freeArb[F, A]))(Suspend[F, A](_)).arbitrary
    ))

  implicit val binaryTreeEq = new (Equal ~> ({type λ[α] = Equal[PairOpt[α]]})#λ){
    def apply[A](a: Equal[A]) = {
      implicit val a0 = a
      Equal[PairOpt[A]]
    }
  }

  checkAll(traverse.laws[PairOpt])
  checkAll(equal.laws[PairOpt[Int]])

  checkAll(traverse.laws[BinaryTree])
  checkAll(equal.laws[BinaryTree[Int]])
}

