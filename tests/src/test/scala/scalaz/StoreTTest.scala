package scalaz

import std.AllInstances._

object StoreTTest extends Scalaprops {

  implicit def storeTuple1IntEqual = new Equal[StoreT[Tuple1, Int, Int]] {
    def equal(a1: StoreT[Tuple1, Int, Int], a2: StoreT[Tuple1, Int, Int]) = (a1.run, a2.run) match {
      case ((tf1, x1), (tf2, x2)) => Equal[Int].equal(x1, x2) && Equal[Int].equal(tf1._1(0), tf2._1(0))
    }
  }

  val testLaws = laws.comonad.all[StoreT[Tuple1, Int, ?]]

  object instances {
    type A = Int
    def functor[F[_] : Functor] = Functor[StoreT[F, A, ?]]
    def cobind[F[_] : Cobind] = Cobind[StoreT[F, A, ?]]
    def comonad[F[_] : Comonad] = Comonad[StoreT[F, A, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Comonad] = Functor[StoreT[F, A, ?]]
    def cobind[F[_] : Comonad] = Cobind[StoreT[F, A, ?]]
  }

}
