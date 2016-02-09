package scalaz

import Property.forAll
import std.string._

object CaseInsensitiveTest extends Scalaprops {
  private[this] implicit val stringGen =
    Tag.unsubst(Gen[String @@ GenTags.AlphaNum])

  val `map identity` = forAll {
    (a: CaseInsensitive[String]) =>
      Equal[CaseInsensitive[String]].equal(a.map(x => x), a)
  }
  
  val `map associativity` = forAll {
    (a: CaseInsensitive[String], f: String => String, g: String => String) =>
      Equal[CaseInsensitive[String]].equal(a.map(f).map(g), a.map(g compose f))
  }

  val testLaws = Properties.list(
    laws.monoid.all[CaseInsensitive[String]],
    laws.order.all[CaseInsensitive[String]]
  )
}
