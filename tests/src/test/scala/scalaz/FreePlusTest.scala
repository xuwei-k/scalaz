package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck._

object FreePlusTest extends SpecLite {

  type FreePlusOption[A] = FreePlus[Option, A]

  trait Template[F[_], G[_]] extends (G ~> ({type λ[α] = G[F[α]]})#λ) {
    override final def apply[A](a: G[A]) = lift(a)
    def lift[A: G]: G[F[A]]
  }

  implicit def freePlusArb[F[_], A](implicit A: Arbitrary[A], F: Arbitrary ~> ({type λ[α] = Arbitrary[F[α]]})#λ): Arbitrary[FreePlus[F, A]] = {
    import FreePlus._
    def loop(pure: Int, suspend: Int, plus: Int): Arbitrary[FreePlus[F, A]] =
      Arbitrary(Gen.frequency(
        (pure,    Functor[Arbitrary].map(A)(Pure[F, A](_)).arbitrary)
       ,(suspend, Functor[Arbitrary].map(F(loop(pure * 2, suspend / 2, plus / 2)))(Suspend[F, A](_)).arbitrary)
//        (plus,    Functor[Arbitrary].map(loop(pure * 2, suspend / 2, plus / 2))(a => FreePlus.Plus[F, A](IList(a))).arbitrary)
// TODO stack overflow
      ))
    loop(10, 1, 1)
  }

  implicit val optionEq = new Template[Option, Equal] {
    def lift[A: Equal] = Equal[Option[A]]
  }

  implicit val optionArb = new Template[Option, Arbitrary] {
    def lift[A: Arbitrary] = Arbitrary(Gen.frequency(
      (1, None)
//      (1, Functor[Arbitrary].map(implicitly[Arbitrary[A]])(Option.apply).arbitrary)
    ))
  }

  checkAll(monadPlus.laws[FreePlusOption])
}

