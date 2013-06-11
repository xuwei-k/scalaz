package scalaz
package iteratee

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class InputTest extends Spec {

  checkAll(equal.laws[Input[Int]])
  checkAll(monoid.laws[Input[Int]])
  checkAll(monadPlus.laws[Input])
  checkAll(traverse.laws[Input])

}
