package scalaz

import org.scalacheck.{Arbitrary, Gen}
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary

object CofreeTTest extends SpecLite {

  type CofreeTOptNel[A] = CofreeT[Option, NonEmptyList, A]

  implicit def cofreeTArb: Arbitrary[CofreeTOptNel[Int]] = {
    Arbitrary(
      for{
        z1 <- Gen.choose(0, 10)
        x <- Gen.choose(30, 50)
        y <- Gen.choose(10, 20)
      }yield
        CofreeT.coiterT(NonEmptyList(z1))(a =>
          if(0 < a.head && a.head < x)
            Some(
              NonEmptyList.nel(a.head + y, a.tail.map(_ + util.Random.nextInt(4)))
            )
          else
            None
        )
    )
  }

  implicit val cofreeTEqual: Equal[CofreeTOptNel[Int]] =
    Equal.equalA[CofreeTOptNel[Int]]

  checkAll(comonad.laws[CofreeTOptNel])
  checkAll(traverse.laws[CofreeTOptNel])

}


