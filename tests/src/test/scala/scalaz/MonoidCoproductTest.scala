package scalaz

import std.anyVal._

object MonoidCoproductTest extends Scalaprops {
  val testLaws = Properties.list(
    laws.monoid.all[Int :+: Byte],
    laws.equal.all[Int :+: Byte]
  )
}
