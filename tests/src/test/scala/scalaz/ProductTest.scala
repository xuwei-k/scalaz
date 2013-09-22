package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class ProductTest extends Spec {
  type OptionList[α] = (Option[α], List[α])
  type NelPair[α] = (NonEmptyList[α], NonEmptyList[α])

  implicit val optionListApplicative = ApplicativePlus[Option].product[List]
  implicit val nelPairTraverse1 = Traverse1[NonEmptyList].product[NonEmptyList]
  implicit val optionListZip = Zip[Option].product[List]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
// TODO checkAll(traverse1.laws[NelPair]) travis test failure
  checkAll(foldable1.laws[NelPair])
  checkAll(zip.laws[OptionList])

  {
    implicit val optionListTraverse = Traverse[Option].product[List]
    checkAll(traverse.laws[OptionList])
  }

  {
    implicit val optionListAlign = Align[Option].product[List]
    checkAll(align.laws[OptionList])
  }

  implicit val eitherTuple2 = Bitraverse[Either].product[Tuple2]
  checkAll(bitraverse.laws[({type λ[α, β] = (Either[α, β], (α, β))})#λ])
}
