package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class ImmutableArrayTest extends Spec {

  checkAll(plus.laws[ImmutableArray])
  checkAll(monoid.laws[ImmutableArray[Int]])
  checkAll(order.laws[ImmutableArray[Int]])

}
