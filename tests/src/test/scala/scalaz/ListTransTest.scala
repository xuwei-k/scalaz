package scalaz

import org.scalacheck._
import scalaz.scalacheck.ScalaCheckBinding._
import Operational._

object ListTransTest extends SpecLite {

  implicit def PromptTArb[I[_], M[_], A](implicit
    A: Arbitrary[A],
    I: Arbitrary[I[A]],
    P: Arbitrary[A => ProgramT[I, M, A]]
  ): Arbitrary[PromptT[I, M, A]] = Arbitrary(Gen.oneOf(
    Functor[Arbitrary].map(A)(PromptT.return_[I, M, A]).arbitrary,
    Apply[Arbitrary].apply2(I, P)(PromptT.bind(_)(_)).arbitrary
  ))

  implicit def ProgramTArb[I[_], M[_], A](implicit
    M: Arbitrary[M[A]],
    P: Arbitrary[ProgramT[I, M, A]],
    F: Arbitrary[A => ProgramT[I, M, A]],
    I: Arbitrary[I[A]]
  ): Arbitrary[ProgramT[I, M, A]] = Arbitrary(Gen.oneOf(
    Functor[Arbitrary].map(M)(ProgramT.lift[I, M, A]).arbitrary,
    Apply[Arbitrary].apply2(P, F)(ProgramT.bind[I, M, A, A](_)(_)).arbitrary,
    Functor[Arbitrary].map(I)(ProgramT.instr[I, M, A]).arbitrary
  ))

  implicit def PlusIArb[M[_], A](implicit
    A: Arbitrary[ListTrans[M, A]]
  ): Arbitrary[PlusI[M, A]] =
    Apply[Arbitrary].apply2(A, A)(PlusI.Plus[M, A])

}

