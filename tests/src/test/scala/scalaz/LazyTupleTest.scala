package scalaz

import std.anyVal._

object LazyTupleTest extends Scalaprops {

  type A = Int
  type B = Int
  type C = Int
  type D = Int

  val bitraverse = laws.bitraverse.all[LazyTuple2]
  val associative = laws.associative.all[LazyTuple2]

  val lazyTuple2 = Properties.list(
    laws.order.all[LazyTuple2[A, B]],
    laws.monoid.all[LazyTuple2[A, B]],
    laws.monad.all[LazyTuple2[B, ?]]
  )

  val lazyTuple3 = Properties.list(
    laws.order.all[LazyTuple3[A, B, C]],
    laws.monoid.all[LazyTuple3[A, B, C]],
    laws.monad.all[LazyTuple3[B, C, ?]]
  )

  val lazyTuple4 = Properties.list(
    laws.order.all[LazyTuple4[A, B, C, D]],
    laws.monoid.all[LazyTuple4[A, B, C, D]],
    laws.monad.all[LazyTuple4[B, C, D, ?]]
  )

}
