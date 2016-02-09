package scalaz

import std.AllInstances._

object CompositionTest extends Scalaprops {
  type OptionList[α] = Option[List[α]]

  implicit val optionListApplicative = ApplicativePlus[Option].compose[List]
  implicit val optionListTraverse = Traverse[Option].compose[List]
  implicit val oneAndOptNelTraverse = Traverse1[OneAnd[Option, ?]].compose[NonEmptyList]

  val optionList = Properties.list(
    laws.applicativePlus.all[OptionList],
    laws.traverse.all[OptionList]
  )

  val traverse1 = laws.traverse1.all[λ[α => OneAnd[Option, NonEmptyList[α]]]]

  implicit val eitherTuple2 = Bitraverse[Either].compose[Tuple2]
  val eitherTuple2Test = laws.bitraverse.all[λ[(α, β) => Either[(α, β), (α, β)]]]

  implicit val listEitherBitraverse = Traverse[List].bicompose[Either]
  val listEitherTest = laws.bitraverse.all[λ[(α, β) => List[Either[α, β]]]]
}
