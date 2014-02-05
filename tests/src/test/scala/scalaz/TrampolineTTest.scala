package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties.monad
import scalaz.scalacheck.ScalaCheckBinding._
import FreeTest._

object TrampolineTTest extends SpecLite {
  
  implicit def coroutineArb[M[_], A](implicit
    M: Arbitrary ~> ({type λ[α] = Arbitrary[M[α]]})#λ,
    N: Functor[M],
    A: Arbitrary[A]
  ): Arbitrary[TrampolineT[M, A]] =
    Arbitrary(Gen.oneOf(
      Functor[Arbitrary].map(M(A))(TrampolineT.done).arbitrary,
      Functor[Arbitrary].map(M(coroutineArb[M, A]))(x =>
        TrampolineT.more(N.map(x)(() => _))
      ).arbitrary
    ))

  type TrampolineTOpt[A]  = TrampolineT[Option, A]
  type TrampolineTList[A] = TrampolineT[List, A]
  type TrampolineTOneAndOpt[A] = TrampolineT[OneAndOpt, A]

  implicit val optArb = new Template[Option, Arbitrary] {
    def lift[A: Arbitrary] = implicitly
  }

  implicit val optEq = new Template[Option, Equal] {
    def lift[A: Equal] = implicitly
  }

  checkAll(monad.laws[TrampolineTOpt])
  checkAll(monad.laws[TrampolineTList])
  checkAll(monad.laws[TrampolineTOneAndOpt])
}

