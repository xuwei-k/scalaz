package scalaz

import Property.forAll

object CordTest extends Scalaprops {
  private[this] implicit val gen =
    Tag.unsubst(Gen[String @@ GenTags.AlphaNum]).map(Cord.stringToCord)

  val `split() must result in two cords whose summary length is equal to the length of original cord` = forAll {
    val x = Cord("Once upon a midnight dreary")
    for (i <- 0 until x.length) {
      val split = x split i
      split._1.length + split._2.length must_== x.length
    }
  }

  val `drop() must make cord shorter` = forAll {
    val theString = "While I pondered, weak and weary"
    val x = Cord(theString)
    for (i <- 0 until x.length) {
      val y = x drop i
      y.toString must_== theString.substring(i)
    }
  }

  val `tail() must be smaller than the whole, generally` = forAll {
    val x = Cord("abc")
    x.tail.toString must_== "bc"
    x.tail.tail.toString must_== "c"
    x.tail.tail.tail.toString must_== ""
  }

  val `isEmpty() must indicate string is empty` = forAll { (a:Cord, b:Cord) =>
    a.isEmpty == a.toString.isEmpty &&
    b.isEmpty == b.toString.isEmpty &&
    (a ++ b).isEmpty == (a.toString.isEmpty && b.toString.isEmpty)
  }

  val `nonEmpty() must indicate string is non-empty` = forAll { (a:Cord, b:Cord) =>
    val c = a ++ b
    a.nonEmpty == !a.toString.isEmpty &&
    b.nonEmpty == !b.toString.isEmpty &&
    (a ++ b).nonEmpty == (!a.toString.isEmpty || !b.toString.isEmpty)
  }

  val testLaws = Properties.list(
    laws.monoid.all[Cord],
    laws.equal.all[Cord]
  )
}
