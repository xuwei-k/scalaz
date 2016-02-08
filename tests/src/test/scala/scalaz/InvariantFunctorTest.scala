package scalaz

import BijectionT.{Bijection, liftBijection}
import Id.Id
import Isomorphism.{<=>, IsoSet}
import std.AllInstances._
import std.option.some
import syntax.invariantFunctor._
import Property.forAll

object InvariantFunctorTest extends Scalaprops {

  val xmap = forAll {
    some(1).xmap[Int](_ + 1, _ - 1) must_===(some(2))
  }

  val `xmap iso` = forAll {
    val succI: Int <=> Int = new IsoSet[Int, Int] {
      def to = (_: Int) + 1
      def from = (_: Int) - 1
    }
    some(1) xmapi succI must_===(some(2))
  }

  val `xmap bijection` = forAll {
    val succB: Bijection[Int, Int] = liftBijection[Id, Id, Int, Int](_ + 1, _ - 1)
    some(1) xmapb succB must_===(some(2))
  }

  case class Num(x: Int)
  implicit val showNum = Show.showA[Num]
  implicit val eqNum = Equal.equalA[Num]
}
