package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Arbitrary

class CompositionTest extends Spec {
  type OptionList[α] = Option[List[α]]
  type OrderShow[α] = Order[Show[α]]
  type OrderList[α] = Order[List[α]]
  type NelShow[α] = NonEmptyList[Show[α]]

  implicit val optionListApplicative = ApplicativePlus[Option].compose[List]
  implicit val optionListTraverse = Traverse[Option].compose[List]
  implicit val orderShow = Contravariant[Order].compose[Show]
  implicit val orderList = Contravariant[Order].icompose[List]
  implicit val nelShow = Functor[NonEmptyList].icompose[Show]

  checkAll(applicative.laws[OptionList])
  checkAll(plusEmpty.laws[OptionList])
  checkAll(traverse.laws[OptionList])

// TODO
//  checkAll(functor.laws[OrderShow])
//  checkAll(contravariant.laws[OrderList])
//  checkAll(contravariant.laws[NelShow])

  implicit val eitherTuple2 = Bitraverse[Either].compose[Tuple2]
  checkAll(bitraverse.laws[({type λ[α, β]=Either[(α, β), (α, β)]})#λ])
}
