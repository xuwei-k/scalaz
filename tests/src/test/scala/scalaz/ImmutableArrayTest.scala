package scalaz

import std.anyVal._
import Property.forAll

object ImmutableArrayTest extends Scalaprops {

  val `Issue #525` = forAll {
    val xs = ImmutableArray.fromArray(Array(1)) ++ ImmutableArray.fromArray(Array("a"))
    xs.toArray.toList must_==(Array(1, "a").toList)
  }

  val `Issue #812` = forAll {
    val xs = ImmutableArray.fromArray(Array("test"))
    val t = xs.tail
    t.toArray.toList must_==(Array().toList)
  }

  val testLaws = Properties.list(
    laws.equal.all[ImmutableArray[Int]],
    laws.foldable.all[ImmutableArray],
    laws.foldable.anyAndAllLazy[ImmutableArray]
  )

}
