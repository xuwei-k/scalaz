package scalaz
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.anyVal._

object ImmutableArrayTest extends SpecLite {

  "Issue #525" in {
    val xs = ImmutableArray.fromArray(Array(1)) ++ ImmutableArray.fromArray(Array("a"))
    xs.toArray.toList must_==(Array(1, "a").toList)
  }

  "Issue #812" in {
    val xs = ImmutableArray.fromArray(Array("test"))
    val t = xs.tail
    t.toArray.toList must_==(Array().toList)
  }

  "WrappedImmutableArray" in {
    val xs = ImmutableArray.fromArray(Array(1, 2, 3))
    val xs1: ImmutableArray[Int] = xs.map(_ + 1)
    val xs2: ImmutableArray[Int] = xs.flatMap(x => ImmutableArray.fromArray(Array(x)))
  }

  checkAll(equal.laws[ImmutableArray[Int]])
  checkAll(foldable.laws[ImmutableArray])
  checkAll(FoldableTests.anyAndAllLazy[ImmutableArray])

}
