package scalaz

import std.AllInstances._

object LazyEitherTest extends Scalaprops {
  implicit def LazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] = new Equal[LazyEither[A, B]] {
    def equal(a: LazyEither[A, B], b: LazyEither[A, B]) =
      Equal[Either[A, B]].equal(a.toEither,b.toEither)
  }

  val testLaws = Properties.list(
    laws.equal.all[LazyEither[Int, Int]],
    laws.monadError.all[LazyEither[Int, ?], Int],
    laws.bindRec.all[LazyEither[Int, ?]],
    laws.traverse.all[LazyEither[Int, ?]]
  )

  val bitraverse = laws.bitraverse.all[LazyEither]
  val associative = laws.associative.all[LazyEither]

  val `tail recursive tailrecM` = Property.forAll {
    val times = 10000
    
    val result = 
      BindRec[LazyEither[Int, ?]].tailrecM[Int, Int] { 
        i => LazyEither.lazyRight(if (i < 10000) \/.left(i + 1) else \/.right(i)) 
      }(0)
    result.getOrElse(0) must_=== times
  }
}
