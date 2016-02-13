package scalaz

import std.AllInstances._
import Property.forAll

object ConstTest extends Scalaprops {
  val order = laws.order.all[Const[Int, String]]

  val list = laws.applicative.all[Const[List[Int], ?]]
  val option = laws.applicative.all[Const[Option[Int], ?]]

  val testLaws = Properties.list(
    laws.traverse.all[Const[Int, ?]],
    laws.contravariant.all[Const[Int, ?]]
  )

  val const = forAll { (x: Int, y: Function0[Int]) =>
    Const.const(x)(y) == x
  }

  object instances {
    def functor[C] = Functor[Const[C, ?]]
    def traverse[C] = Traverse[Const[C, ?]]
    def functorMax[C: Monoid] = Functor[Const[C, ?]]
    def apply[C: Semigroup] = Apply[Const[C, ?]]
    def applicative[C: Monoid] = Applicative[Const[C, ?]]
    def equal[C: Equal, A] = Equal[Const[C, A]]
    def equalMax[C: Order, A] = Equal[Const[C, A]]
    def order[C: Order, A] = Order[Const[C, A]]
  }
}
