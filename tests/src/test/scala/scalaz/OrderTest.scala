package scalaz

import std.AllInstances._
import Dual._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Arbitrary, Gen}

class OrderTest extends Spec {

  implicit val orderIntArb: Arbitrary[Order[Int]] = Arbitrary(Gen.oneOf(
    Order[Int], Order[Int].reverseOrder
  ))
  implicit val orderEqual: Equal[Order[Int]] = Equal.equal{ (a1, a2) =>
    Iterator.fill(100)(scala.util.Random.nextInt).sliding(2).forall{
      case List(x, y) => a1.order(x, y) == a2.order(x, y)
    }
  }

  checkAll(contravariant.laws[Order])

  "duals" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
      val xsdual: List[Int @@ Tags.Dual] = Tag subst xs
      (F maximum xs: Option[Int]) must be_===(F minimum xsdual: Option[Int])
      (F minimum xs: Option[Int]) must be_===(F maximum xsdual: Option[Int])
  }

  "semigroups min" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      (xs map MinVal).suml1 must be_===(F minimum1 xs)
  }

  "semigroups max" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      (xs map MaxVal).suml1 must be_===(F maximum1 xs)
  }
}
