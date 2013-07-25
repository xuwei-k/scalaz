package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ProductTest extends Spec {

  type OptionList[a] = (Option[a], List[a])
  type OptionNel[a] = (Option[a], NonEmptyList[a])
  type NelList[a] = (NonEmptyList[a], List[a])

  implicit val optionListApplicative = ApplicativePlus[Option].product[List]
  implicit val optionListTraverse = Traverse[Option].product[List]
  implicit val optionNelTraverse1: Traverse1[OptionNel] = Traverse[Option].product0[NonEmptyList]
  implicit val nelListTraverse1: Traverse1[NelList] = Traverse1[NonEmptyList].product0[List]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(traverse.laws[OptionList])
  checkAll(traverse.laws[OptionNel])
  checkAll(traverse.laws[NelList])

  implicit val eitherTuple2 = Bitraverse[Either].product[Tuple2]
  checkAll(bitraverse.laws[({type λ[α, β]=(Either[α, β], Tuple2[α, β])})#λ])
}
