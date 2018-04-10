// Copyright (C) 2017 John A. De Goes. All rights reserved.
package scalaz
package ioeffect

import scalaz.std.AllInstances._
import scala.concurrent.duration._

object RTSSpec extends SpecLite with RTS {

  "Point" in {
    unsafePerformIO(IO.point(1)) must_=== 1
  }

  "PointIsLazy" in {
    IO.point(throw new Error("Not lazy"))
    true
  }

  "NowIsEager" in {
    (IO.now(throw new Error("Eager"))).mustThrowA[Error]
  }

  "SuspendIsLazy" in {
    IO.suspend(throw new Error("Eager"))
    true
  }

  "SuspendIsEvaluatable" in {
    unsafePerformIO(IO.suspend(IO.point(42))) must_=== 42
  }

  "SyncEvalLoop" in {
    def fibIo(n: Int): IO[BigInt] =
      if (n <= 1) IO.point(n)
      else
        for {
          a <- fibIo(n - 1)
          b <- fibIo(n - 2)
        } yield a + b

    unsafePerformIO(fibIo(10)) must_=== fib(10)
  }

  "EvalOfSyncEffect" in {
    def sumIo(n: Int): IO[Int] =
      if (n <= 0) IO.sync(0)
      else IO.sync(n).flatMap(b => sumIo(n - 1).map(a => a + b))

    unsafePerformIO(sumIo(1000)) must_=== sum(1000)
  }

  "EvalOfAttemptOfSyncEffectError" in {
    unsafePerformIO(IO.sync(throw ExampleError).attempt) must_== -\/(ExampleError)
  }

  "EvalOfAttemptOfFail" in {
    unsafePerformIO(IO.fail[Int](ExampleError).attempt) must_== -\/(ExampleError)

    unsafePerformIO(IO.suspend(IO.suspend(IO.fail[Int](ExampleError)).attempt)) must_== -\/(ExampleError)
  }

  "AttemptOfDeepSyncEffectError" in {
    unsafePerformIO(deepErrorEffect(100).attempt) must_== -\/(ExampleError)
  }

  "AttemptOfDeepFailError" in {
    unsafePerformIO(deepErrorFail(100).attempt) must_== -\/(ExampleError)
  }

  "EvalOfUncaughtFail" in {
    unsafePerformIO(IO.fail[Int](ExampleError)).mustThrowA[ExampleError]
  }

  "EvalOfUncaughtThrownSyncEffect" in {
    unsafePerformIO(IO.sync[Int](throw ExampleError)).mustThrowA[ExampleError]
  }

  "EvalOfDeepUncaughtThrownSyncEffect" in {
    unsafePerformIO(deepErrorEffect(100)).mustThrowA[ExampleError]
  }

  "EvalOfDeepUncaughtFail" in {
    unsafePerformIO(deepErrorEffect(100)).mustThrowA[ExampleError]
  }

  "EvalOfFailEnsuring" in {
    var finalized = false

    unsafePerformIO(IO.fail[Unit](ExampleError).ensuring(IO.sync[Unit] { finalized = true; () })).mustThrowA[ExampleError]
    finalized must_=== true
  }

  "EvalOfFailOnError" in {
    var finalized = false

    unsafePerformIO(IO.fail[Unit](ExampleError).onError(_ => IO.sync[Unit] { finalized = true; () })).mustThrowA[ExampleError]

    finalized must_=== true
  }

  "ErrorInFinalizerCannotBeCaught" in {
    val nested: IO[Int] =
      IO.fail(ExampleError).ensuring(IO.fail(new Error("e2"))).ensuring(IO.fail(new Error("e3")))

    unsafePerformIO(nested).mustThrowA[ExampleError]
  }

  "ErrorInFinalizerIsReported" in {
    var reported: Throwable = null

    unsafePerformIO {
      IO.point(42).ensuring(IO.fail(ExampleError)).fork0(e => IO.sync[Unit] { reported = e; () })
    }

    // FIXME: Is this an issue with thread synchronization?
    while (reported == null) Thread.`yield`()

    reported must_== ExampleError
  }

  "BracketResultIsUsageResult" in {
    unsafePerformIO(IO.unit.bracket_(IO.unit)(IO.point(42))) must_=== 42
  }

  "BracketErrorInAcquisition" in {
    unsafePerformIO(IO.fail[Unit](ExampleError).bracket_(IO.unit)(IO.unit)).mustThrowA[ExampleError]
  }

  "BracketErrorInRelease" in {
    unsafePerformIO(IO.unit.bracket_(IO.fail[Unit](ExampleError))(IO.unit)).mustThrowA[ExampleError]
  }

  "BracketErrorInUsage" in {
    unsafePerformIO(IO.unit.bracket_(IO.unit)(IO.fail[Unit](ExampleError))).mustThrowA[ExampleError]
  }

  "BracketRethrownCaughtErrorInAcquisition" in {
    lazy val actual = unsafePerformIO(IO.absolve(IO.fail[Unit](ExampleError).bracket_(IO.unit)(IO.unit).attempt))

    actual.mustThrowA[ExampleError]
  }

  "BracketRethrownCaughtErrorInRelease" in {
    lazy val actual = unsafePerformIO(IO.absolve(IO.unit.bracket_(IO.fail[Unit](ExampleError))(IO.unit).attempt))

    actual.mustThrowA[ExampleError]
  }

  "BracketRethrownCaughtErrorInUsage" in {
    lazy val actual = unsafePerformIO(IO.absolve(IO.unit.bracket_(IO.unit)(IO.fail[Unit](ExampleError)).attempt))

    actual.mustThrowA[ExampleError]
  }

  "EvalOfAsyncAttemptOfFail" in {
    val io1 = IO.unit.bracket_(AsyncUnit)(asyncExampleError[Unit])
    val io2 = AsyncUnit.bracket_(IO.unit)(asyncExampleError[Unit])

    unsafePerformIO(io1).mustThrowA[ExampleError]
    unsafePerformIO(io2).mustThrowA[ExampleError]
    unsafePerformIO(IO.absolve(io1.attempt)).mustThrowA[ExampleError]
    unsafePerformIO(IO.absolve(io2.attempt)).mustThrowA[ExampleError]
  }

  "EvalOfDeepSyncEffect" in {
    def incLeft(n: Int, ref: IORef[Int]): IO[Int] =
      if (n <= 0) ref.read
      else incLeft(n - 1, ref) <* ref.modify(_ + 1)

    def incRight(n: Int, ref: IORef[Int]): IO[Int] =
      if (n <= 0) ref.read
      else ref.modify(_ + 1) *> incRight(n - 1, ref)

 // TODO "AssertionError: 0 !== 100"
 //
 // unsafePerformIO(for {
 //   ref <- IORef(0)
 //   v   <- incLeft(100, ref)
 // } yield v) must_=== 100
 //

    unsafePerformIO(for {
      ref <- IORef(0)
      v   <- incRight(1000, ref)
    } yield v) must_=== 1000
  }

  "DeepMapOfPoint" in {
    unsafePerformIO(deepMapPoint(10000)) must_=== 10000
  }

  "DeepMapOfNow" in {
    unsafePerformIO(deepMapNow(10000)) must_=== 10000
  }

  "DeepMapOfSyncEffectIsStackSafe" in {
    unsafePerformIO(deepMapEffect(10000)) must_=== 10000
  }

  "DeepAttemptIsStackSafe" in {
    unsafePerformIO((0 until 10000).foldLeft(IO.sync(())) { (acc, _) =>
      acc.attempt.toUnit
    }) must_=== (())
  }

  "DeepBindOfAsyncChainIsStackSafe" in {
    val result = (0 until 10000).foldLeft(IO.point(0)) { (acc, _) =>
      acc.flatMap(n => IO.async[Int](_(\/-(n + 1))))
    }

    unsafePerformIO(result) must_=== 10000
  }

  "AsyncEffectReturns" in {
    unsafePerformIO(IO.async[Int](cb => cb(\/-(42)))) must_=== 42
  }

  "SleepZeroReturns" in {
    unsafePerformIO(IO.sleep(1.nanoseconds)) must_=== ((): Unit)
  }

  "ForkJoinIsId" in {
    unsafePerformIO(IO.point(42).fork.flatMap(_.join)) must_=== 42
  }

  "DeepForkJoinIsId" in {
    val n = 20

    unsafePerformIO(concurrentFib(n)) must_=== fib(n)
  }

  "NeverIsInterruptible" in {
    val io =
      for {
        fiber <- IO.never[Int].fork
        _     <- fiber.interrupt(ExampleError)
      } yield 42

    unsafePerformIO(io) must_=== 42
  }

  "RaceOfValueNever" in {
    unsafePerformIO(IO.point(42).race(IO.never[Int])) == 42
  }

  // Utility stuff
  class ExampleError extends Error("Oh noes!")
  object ExampleError extends ExampleError

  def asyncExampleError[A]: IO[A] = IO.async[A](_(-\/(ExampleError)))

  def sum(n: Int): Int =
    if (n <= 0) 0
    else n + sum(n - 1)

  def deepMapPoint(n: Int): IO[Int] =
    if (n <= 0) IO.point(n) else IO.point(n - 1).map(_ + 1)

  def deepMapNow(n: Int): IO[Int] =
    if (n <= 0) IO.now(n) else IO.now(n - 1).map(_ + 1)

  def deepMapEffect(n: Int): IO[Int] =
    if (n <= 0) IO.sync(n) else IO.sync(n - 1).map(_ + 1)

  def deepErrorEffect(n: Int): IO[Unit] =
    if (n == 0) IO.sync(throw ExampleError)
    else IO.unit *> deepErrorEffect(n - 1)

  def deepErrorFail(n: Int): IO[Unit] =
    if (n == 0) IO.fail(ExampleError)
    else IO.unit *> deepErrorFail(n - 1)

  def fib(n: Int): BigInt =
    if (n <= 1) n
    else fib(n - 1) + fib(n - 2)

  def concurrentFib(n: Int): IO[BigInt] =
    if (n <= 1) IO.point(n)
    else
      for {
        f1 <- concurrentFib(n - 1).fork
        f2 <- concurrentFib(n - 2).fork
        v1 <- f1.join
        v2 <- f2.join
      } yield v1 + v2

  val AsyncUnit = IO.async[Unit](_(\/-(())))
}
