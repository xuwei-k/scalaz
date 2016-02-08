package scalaz

import std.AllInstances._
import syntax.bind._
import Property.forAll

object BindTest extends Scalaprops {

  val >>= = forAll {
    (List(1, 2, 3) >>= (x => List(x, x))) must_===(List(1, 1, 2, 2, 3, 3))
  }

  val ifM = forAll {
    List(true, false, true).ifM(List(0), List(1, 1)) must_===(List(0, 1, 1, 0))
  }

  val >> = forAll {
    List(1, 2, 3) >> (List(2, 3)) must_===(List(2, 3, 2, 3, 2, 3))
  }

  val join = forAll {
    List(List(1, 2)).join must_===(List(1, 2))
  }
}
