package scalaz

import scalaz.std.anyVal._

class EphemeralStreamTestJVM extends SpecLite {
  "foldLeft large stream" in {
    val list = List.fill(10000000)(1)
    val xs = EphemeralStream(list : _*)
    Foldable[EphemeralStream].foldLeft(xs, 0)(_ + _) must_===(list.sum)
  }
}
