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

  implicit val oneAndOptEq = new Template[Equal, OneAndOpt]{
    def lift[A: Equal] = implicitly
  }

  implicit val oneAndOptArb = new Template[Arbitrary, OneAndOpt]{
    def lift[A: Arbitrary] = implicitly
  }

  type OneAndOpt[A] = OneAnd[Option, A]

  type FreeTOneAndOptList[A]   = FreeT[OneAndOpt, List, A]
  type FreeTOneAndOptOpt[A]    = FreeT[OneAndOpt, Option, A]
  type FreeTOptOneAndOpt[A]    = FreeT[Option, OneAndOpt, A]
  type FreeTListOneAndOpt[A]   = FreeT[List, OneAndOpt, A]

  type FreeTOptOpt[A]          = FreeT[Option, Option, A]
  type FreeTOptList[A]         = FreeT[Option, List, A]
  type FreeTListOpt[A]         = FreeT[List, Option, A]
  type FreeTListList[A]        = FreeT[List, List, A]

  checkAll("FreeT[OneAndOpt, List, _]"     , monadPlus.laws[FreeTOneAndOptList])
  checkAll("FreeT[OneAndOpt, Option, _]"   , monadPlus.laws[FreeTOneAndOptOpt])

  checkAll("FreeT[Option, OneAndOpt, _]"   , monad.laws[FreeTOptOneAndOpt])
  checkAll("FreeT[Option, OneAndOpt, _]"   , plus.laws[FreeTOptOneAndOpt])
  checkAll("FreeT[List, OneAndOpt, _]"     , monad.laws[FreeTListOneAndOpt])
  checkAll("FreeT[List, OneAndOpt, _]"     , plus.laws[FreeTListOneAndOpt])

  checkAll("FreeT[Option, Option, _]"      , monadPlus.laws[FreeTOptOpt])
  checkAll("FreeT[Option, List, _]"        , monadPlus.laws[FreeTOptList])
  checkAll("FreeT[List, Option, _]"        , monadPlus.laws[FreeTListOpt])
  checkAll("FreeT[List, List, _]"          , monadPlus.laws[FreeTListList])

  property("FreeT[F, Id, A] is Free[F, A]") = forAll{ a: Free[List, Int] =>
    implicit val s = Show.showA[Free[List, Int]]
    FreeT.fromFree[List, Id.Id, Int](a).toFree must_=== a
  }

  property("no stack overflow") = {
    import scalaz.Free.Trampoline

    val n = 100000

    def isEven(xs: List[Int]): Trampoline[Boolean] =
      if (xs.isEmpty) Trampoline.done(true) else Trampoline.suspend(isOdd(xs.tail))
    def isOdd(xs: List[Int]): Trampoline[Boolean] =
      if (xs.isEmpty) Trampoline.done(false) else Trampoline.suspend(isEven(xs.tail))

    def foo = FreeT.fromFreeId(isEven((1 to n).toList))

    FreeT.fromFree((1 to n).map(x => scalaz.Free.liftF(Option(x))).toList).iterT(_ getOrElse Nil) must_=== (1 to n).toList
    foo.toFree.run must_=== true
    foo.map(x => !x).toFree.run must_=== false

//    FreeT.fromFree[Function0, Id.Id, Boolean](isEven((1 to 100000).toList)).iterT(_()) must_=== true
  }

  property("no stack overflow") = {
    import scalaz.Free.Trampoline
    val n = 100000

    type T = FreeT[Function0, Id.Id, Boolean]

    def isEven(xs: List[Int]): T =
      if (xs.isEmpty) FreeT.fromFreeId(Trampoline.done(true)) else FreeT.suspend(isOdd(xs.tail))
    def isOdd(xs: List[Int]): T =
      if (xs.isEmpty) FreeT.fromFreeId(Trampoline.done(false)) else FreeT.suspend(isEven(xs.tail))

    def foo = isEven((1 to n).toList)

    foo.toFree.run must_=== true
  }

  property("fib") = {
    type T = FreeT[Function0, Id.Id, Int]

    def fib(n: Int): T =
      if (n < 2) FreeT.fromFreeId(Trampoline.done(n)) else for {
        x <- FreeT.suspend(fib(n - 1))
        y <- FreeT.suspend(fib(n - 2))
      } yield (x + y)

    fib(30).toFree.run must_=== 832040
  }
}

