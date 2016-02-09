package scalaz

import std.AllInstances._

object ProductTest extends Scalaprops {
  type OptionList[α] = (Option[α], List[α])
  type OneAndOption[α] = OneAnd[Option, α]
  type OneAndOptionPair[α] = (OneAndOption[α], OneAndOption[α])

  implicit val optionListMonadPlus = MonadPlus[Option].product[List]
  implicit val optionListZip = Zip[Option].product[List]
  implicit val oneAndOptionPairTraverse1 = Traverse1[OneAndOption].product[OneAndOption]

  val optionListBindRecTest = {
    implicit val optionListBindRec = BindRec[Option].product[List]
    laws.bindRec.tailrecBindConsistency[OptionList, Int]
  }

  val optionList = Properties.list(
    laws.monadPlusStrong.all[OptionList],
    laws.zip.all[OptionList]
  )

  val oneAndOptionPair = laws.traverse1.all[OneAndOptionPair]

  implicit val eitherTuple2 = Bitraverse[Either].product[Tuple2]
  val eitherTuple2Test = laws.bitraverse.all[λ[(α, β) => (Either[α, β], (α, β))]]
}
