package scalaz

import std.AllInstances._
import Property.forAll

object MonoidTest extends Scalaprops {
  val multiply = forAll{ (a: Int, b: Int) =>
    if(b <= 0) {
      Monoid[Int].multiply(a, b) must_=== 0
    } else {
      Monoid[Int].multiply(a, b) must_=== (a * b)
    }
  }

  val `endo multiply` = forAll {
    import syntax.monoid._

    def inc(i: Int) = i + 1

    val incTimesThree: Endo[Int] = Endo(inc).multiply(3)
    incTimesThree(0) must_===(3)
  }

  val `endo kleisli multiply` = forAll {
    import syntax.monoid._

    val k = Kleisli { i: Int => if (i % 2 == 0) Some(i * 2) else None }

    val kTimes3 = k.endo.multiply(3)
    kTimes3.run(2) must_=== (Some(16))
  }

  val unfold = forAll {
    val ss = std.stream.unfold(1) {
      case x if x < 10 => Some((x.toString, x * 2))
      case _           => None
    }
    ss.toList must_===(List("1", "2", "4", "8"))
  }

  val `intercalate empty` = forAll {
    Foldable[List].intercalate(List[String](), "oops") must_===("")
  }

  val intercalate = forAll {
    val xs = List(Vector(Cord("this"), Cord("has")), Vector(),
		  Vector(Cord("elements")), Vector(Cord("beneath")), Vector())
    ((Foldable[List] compose Foldable[Vector]).intercalate(xs, Cord("!!")).toString
     must_===(Cord("this!!has!!elements!!beneath").toString))
  }

  val `invariant functor` = forAll {
    import InvariantFunctorTest._
    import syntax.invariantFunctor._

    val sg: Monoid[Num] = Monoid[Int].xmap[Num](Num.apply _, _.x)
    sg.append(Num(1), Num(2)) must_===(Num(3))
    sg.zero must_===(Num(0))
  }
}
