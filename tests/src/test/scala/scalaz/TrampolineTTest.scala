package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties.monad
import scalaz.scalacheck.ScalaCheckBinding._
import FreeTest._
import Id.Id

object TrampolineTTest extends SpecLite {

  implicit def trampolineTArb[M[_], A](implicit
    M: Arbitrary ~> ({type λ[α] = Arbitrary[M[α]]})#λ,
    N: Functor[M],
    A: Arbitrary[A]
  ): Arbitrary[TrampolineT[M, A]] =
    Arbitrary(Gen.oneOf(
      Functor[Arbitrary].map(M(A))(TrampolineT.done).arbitrary,
      Functor[Arbitrary].map(M(trampolineTArb[M, A]))(x =>
        TrampolineT.more(N.map(x)(() => _))
      ).arbitrary
    ))

  type TrampolineTOpt[A]  = TrampolineT[Option, A]
  type TrampolineTList[A] = TrampolineT[List, A]
  type TrampolineTOneAndOpt[A] = TrampolineT[OneAndOpt, A]

  implicit val optArb = new FreeTest.Template[Option, Arbitrary] {
    def lift[A: Arbitrary] = implicitly
  }

  implicit val optEq = new FreeTest.Template[Option, Equal] {
    def lift[A: Equal] = implicitly
  }

  checkAll(monad.laws[TrampolineTOpt])
  checkAll(monad.laws[TrampolineTList])
  checkAll(monad.laws[TrampolineTOneAndOpt])

  private val fibResult = 317811
  private val fibParam = 28

  "fibonacci" ! {
    def fib(n: Int): TrampolineT[Id, Int] =
      if (n < 2) TrampolineT.done[Id, Int](n)
      else TrampolineT.trampolineTMonad[Id].apply2(
        TrampolineT.suspend(fib(n - 1)),
        TrampolineT.suspend(fib(n - 2))
      )(_ + _)

    fib(fibParam).run must_=== fibResult

    val nt = new (Id ~> Option){def apply[A](a: A) = Option(a)}
    fib(fibParam).trans(nt).run must_=== Option(fibResult)
  }

  "from" ! {
    def fib(n: Int): Free.Trampoline[Int] =
      if (n < 2) Trampoline.done(n)
      else Apply[Free.Trampoline].apply2(
        Trampoline.suspend(fib(n - 1)),
        Trampoline.suspend(fib(n - 2))
      )(_ + _)

    TrampolineT.from[Id, Int](fib(fibParam)).run must_=== fibResult
    fib(fibParam).run must_=== fibResult
  }

  "kleisli" ! {
    val a = 100000
    val b = 10
    val k = Kleisli[Option, IList[Int], IList[Int]](_.tailOption)
    val endo = k.liftMK[TrampolineT].endo
    val m = Endomorphic.kleisliEndoInstance[({type λ[α] = TrampolineT[Option, α]})#λ, IList[Int]]
    m.multiply(endo, a).run.run(IList.fill(a + b)(0)).run.map(_.length) must_=== Option(b)
  }

  "no stack overflow" should {
    val a = 1 to 100000
    import syntax.foldable._

    "Foldable#foldLeftMTrampoline" ! {
      val f1 = (x: Int, y: Int) => Option(x + y)
      a.toList.foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)
      a.toStream.foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)
      EphemeralStream(a: _*).foldLeftMTrampoline(0)(f1) must_=== Some(a.sum)

      a.toList.foldLeftMTrampoline[Id, Vector[Int]](Vector[Int]())(_ :+ _) must_=== a.toList.foldLeft(Vector[Int]())(_ :+ _)
    }

    "Foldable#foldRightMTrampoline" ! {
      val f1: (Int, => Int) => Option[Int] = (x, y) => Option(x + y)
      a.toList.foldRightMTrampoline(0)(f1) must_=== Some(a.sum)
      a.toStream.foldRightMTrampoline(0)(f1) must_=== Some(a.sum)
      EphemeralStream(a: _*).foldRightMTrampoline(0)(f1) must_=== Some(a.sum)

      a.toList.foldRightMTrampoline[Id, List[Int]](List[Int]())(_ :: _) must_=== Foldable[List].foldRight(a.toList, List[Int]())(_ :: _)
    }
  }
}

