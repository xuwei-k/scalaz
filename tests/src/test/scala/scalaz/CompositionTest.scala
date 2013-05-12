package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class CompositionTest extends Spec {
  type OptionList[α] = Option[List[α]]

  implicit val optionList = MonadPlus[Option].compose[List]

  checkAll(monadPlus.laws[OptionList])
}
