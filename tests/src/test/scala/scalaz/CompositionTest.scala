package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class CompositionTest extends Spec {
  type OptionList[α] = Option[List[α]]
  type NelNel[α] = NonEmptyList[NonEmptyList[α]]

  implicit val optionListApplicative = ApplicativePlus[Option].compose[List]
  implicit val optionListTraverse = Traverse[Option].compose[List]
  implicit val nelNelTraverse1 = Traverse1[NonEmptyList].compose[NonEmptyList]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(traverse.laws[OptionList])
  checkAll(traverse1.laws[NelNel])


  implicit val eitherTuple2 = Bitraverse[Either].compose[Tuple2]
  checkAll(bitraverse.laws[({type λ[α, β]=Either[(α, β), (α, β)]})#λ])
}
