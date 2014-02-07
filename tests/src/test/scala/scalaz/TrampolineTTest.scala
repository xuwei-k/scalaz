package scalaz

import org.scalacheck.{Arbitrary, Gen}
import Isomorphism._
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties.monad
import scalaz.scalacheck.ScalaCheckBinding._
import FreeTest._

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

  implicit val optArb = new Template[Option, Arbitrary] {
    def lift[A: Arbitrary] = implicitly
  }

  implicit val optEq = new Template[Option, Equal] {
    def lift[A: Equal] = implicitly
  }

  checkAll(monad.laws[TrampolineTOpt])
  checkAll(monad.laws[TrampolineTList])
  checkAll(monad.laws[TrampolineTOneAndOpt])

  "no stack overflow" ! {
    def idFunctions = Iterator.continually((x: Int) => x).take(100000)

    val x1 = TrampolineT.done(Option(1))
    idFunctions.foldLeft(x1)(_ map _).go must_=== Option(1)

    val x2 = TrampolineT.done(List(1))
    idFunctions.foldLeft(x2)(_ map _).go must_=== List(1)

    val f1 = (_: Int) :: Nil
    val f2 = (x: Int) => x :: x :: Nil
    val functions = List(f1, f2).map(f => (x: Int) => TrampolineT.delay(f(x)))
    val n = 16
    val bindFunctions = List.fill(n)(functions).flatten
    bindFunctions.foldLeft(x2)(_ flatMap _).go.size must_=== List.fill(n)(2).product
  }

  private val fibResult = 75025
  private val fibParam = 25

  "fib" ! {
    def fib(n: Int): TrampolineT[Id.Id, Int] =
      if (n < 2) TrampolineT.done[Id.Id, Int](n)
      else TrampolineT.trampolineTMonad[Id.Id].apply2(
        TrampolineT.suspend(fib(n - 1)),
        TrampolineT.suspend(fib(n - 2))
      )(_ + _)

    fib(fibParam).go must_=== fibResult
  }

  "from" ! {
    def fib(n: Int): Free.Trampoline[Int] =
      if (n < 2) Trampoline.done(n)
      else Apply[Free.Trampoline].apply2(
        Trampoline.suspend(fib(n - 1)),
        Trampoline.suspend(fib(n - 2))
      )(_ + _)

    TrampolineT.from[Id.Id, Int](fib(fibParam)).go must_=== fibResult
    fib(fibParam).run must_=== fibResult
  }

  "kleisli" ! {
    val a = 100000
    val b = 10
    val k = Kleisli[Option, IList[Int], IList[Int]](_.tailOption)
    val endo = k.liftMK[TrampolineT].endo
    val m = Endomorphic.kleisliEndoInstance[({type λ[α] = TrampolineT[Option, α]})#λ, IList[Int]]
    m.multiply(endo, a).run.run(IList.fill(a + b)(0)).go.map(_.length) must_=== Option(b)
  }
}

