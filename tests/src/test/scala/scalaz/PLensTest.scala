package scalaz

import std.AllInstances._

object PLensTest extends Scalaprops {
  import PLens._

  val `list head` = Property.forAll {
    listHeadPLens[Int].get(List(1, 2)) must_===(Some(1))
    listHeadPLens[Int].get(Nil) must_===(None)
  }

  object instances {
    def category = Category[PLens]
    def choice = Choice[PLens]
    def split = Split[PLens]
  }

}
