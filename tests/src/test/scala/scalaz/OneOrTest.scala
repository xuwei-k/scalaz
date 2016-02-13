package scalaz

import std.AllInstances._
import Property.forAll

object OneOrTest extends Scalaprops {
  import OneOr._

  val testLaws = Properties.list(
    laws.equal.all[OneOr[List, Int]],
    laws.order.all[OneOr[List, Int]]
  )
  val list = Properties.list(
    laws.traverse.all[OneOrList],
    laws.applicative.all[OneOrList]
  )
  val nel = Properties.list(
    laws.traverse1.all[OneOrNel],
    laws.comonad.all[OneOrNel]
  )

  val findLeft = forAll { (a: OneOr[List, Int], f: Int => Boolean) =>
    val F = Foldable[OneOr.OneOrList]
    F.findLeft(a)(f) must_=== Foldable[List].findLeft(F.toList(a))(f)
  }

  val findRight = forAll { (a: OneOr[List, Int], f: Int => Boolean) =>
    val F = Foldable[OneOr.OneOrList]
    F.findRight(a)(f) must_=== Foldable[List].findRight(F.toList(a))(f)
  }

  object instances {
    def functor[F[_]: Functor] = Functor[OneOr[F, ?]]
    def apply[F[_]: Apply] = Apply[OneOr[F, ?]]
    def applicative[F[_]: Apply] = Applicative[OneOr[F, ?]]
    def cobind[F[_]: Cobind] = Cobind[OneOr[F, ?]]
    def comonad[F[_]: Comonad] = Comonad[OneOr[F, ?]]
    def foldable[F[_]: Foldable] = Foldable[OneOr[F, ?]]
    def foldable1[F[_]: Foldable1] = Foldable1[OneOr[F, ?]]
    def traverse[F[_]: Traverse] = Traverse[OneOr[F, ?]]
    def traverse1[F[_]: Traverse1] = Traverse1[OneOr[F, ?]]
  }

}
