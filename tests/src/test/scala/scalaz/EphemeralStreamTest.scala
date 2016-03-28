package scalaz

import std.AllInstances._
import syntax.contravariant._
import Property.forAll

object EphemeralStreamTest extends Scalaprops {

  val testLaws = Properties.list(
    laws.equal.all[EphemeralStream[Int]],
    laws.bindRec.all[EphemeralStream],
    laws.monadPlusStrong.all[EphemeralStream],
    laws.isEmpty.all[EphemeralStream],
    laws.traverse.all[EphemeralStream],
    laws.zip.all[EphemeralStream],
    laws.align.all[EphemeralStream],
    laws.cobind.all[EphemeralStream]
  )

  implicit def ephemeralStreamShow[A: Show]: Show[EphemeralStream[A]] =
    Show[List[A]].contramap(_.toList)

  val reverse = forAll{ e: EphemeralStream[Int] =>
    e.reverse.toList must_===(e.toList.reverse)
    e.reverse.reverse must_===(e)
  }

  val foldLeft = forAll{ xs: List[List[Int]] =>
    Foldable[EphemeralStream].foldLeft(EphemeralStream(xs: _*), List[Int]())(_ ::: _) must_===(xs.foldLeft(List[Int]())(_ ::: _))
  }

  val `unzip zip` = forAll { xs: EphemeralStream[(Int, Int)] =>
    val (firsts, seconds) = xs.unzip
    (firsts zip seconds) must_===(xs)
  }

  val `zip has right length` = forAll {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs zip ys).length must_===(xs.length min ys.length)
  }

  val `interleave has right length` = forAll {(xs: EphemeralStream[Int], ys: EphemeralStream[Int]) =>
    (xs interleave ys).length must_===(xs.length + ys.length)
  }

  val take = forAll { (xs: Stream[Int], n: Int) =>
    EphemeralStream.fromStream(xs).take(n) must_===(EphemeralStream.fromStream(xs.take(n)))
  }

  val `take from infinite stream` = forAll {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).take(n) must_===(EphemeralStream.fromStream(Stream.iterate(0)(_ + 1).take(n)))
  }

  val takeWhile = forAll { (xs: Stream[Int], n: Int) =>
    EphemeralStream.fromStream(xs).takeWhile(_ < n) must_===(EphemeralStream.fromStream(xs.takeWhile(_ < n)))
  }

  val `takeWhile from infinite stream` = forAll {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).takeWhile(_ < n) must_===(EphemeralStream.fromStream(Stream.iterate(0)(_ + 1).takeWhile(_ < n)))
  }

  val index = forAll {(xs: EphemeralStream[Int], i: Int) =>
    Foldable[EphemeralStream].index(xs, i) must_===(xs.toList.lift.apply(i))
  }

  val `index infinite stream` = forAll {
    val i = util.Random.nextInt(1000)
    val xs = Stream from 0
    Foldable[EphemeralStream].index(EphemeralStream.fromStream(xs), i) must_===(xs.lift.apply(i))
  }

  val inits = forAll { xs: EphemeralStream[Int] =>
    import syntax.std.list._
    xs.inits.map(_.toList).toList must_===(xs.toList.initz)
  }

  val tails = forAll { xs: EphemeralStream[Int] =>
    import syntax.std.list._
    xs.tails.map(_.toList).toList must_===(xs.toList.tailz)
  }

  val `inits infinite stream` = forAll {
    EphemeralStream.iterate(0)(_ + 1).inits
    ()
  }

  val `tails infinite stream` = forAll {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).tails
      .map(t => Foldable[EphemeralStream].toStream(t.take(n)))
      .take(n) must_===(
      EphemeralStream.fromStream(Stream.iterate(0)(_ + 1).tails.map(_ take n).toStream.take(n))
    )
  }

  val `foldMap evaluates lazily` = forAll {
    val infiniteStream = EphemeralStream.iterate(false)(identity)
    Foldable[EphemeralStream].foldMap(infiniteStream)(identity)(booleanInstance.conjunction) must_===(false)
  }

  val `foldRight evaluates lazily` = forAll {
    val infiniteStream = EphemeralStream.iterate(true)(identity)
    Foldable[EphemeralStream].foldRight(infiniteStream, true)(_ || _) must_===(true)
  }

  val zipL = forAll {
    val size = 100
    val infinite = EphemeralStream.iterate(0)(_ + 1)
    val finite = EphemeralStream.range(0, size)
    val F = Traverse[EphemeralStream]
    F.zipL(infinite, infinite)
    F.zipL(finite, infinite).length must_===(size)
    F.zipL(finite, infinite) must_===((finite zip infinite).map{x => (x._1, Option(x._2))})
    F.zipL(infinite, finite).take(1000).length must_===(1000)
    F.zipL(infinite, finite).takeWhile(_._2.isDefined).length must_===(size)
  }

  val zipWithIndex = forAll { (xs: Stream[Int], n: Int) =>
    EphemeralStream.fromStream(xs).take(n).zipWithIndex must_===(EphemeralStream.fromStream(xs.take(n).zipWithIndex))
  }

  val `zipWithIndex from infinite stream` = forAll {
    val n = util.Random.nextInt(1000)
    EphemeralStream.iterate(0)(_ + 1).zipWithIndex.take(n) must_===(
      EphemeralStream.fromStream(Stream.iterate(0)(_ + 1).zipWithIndex.take(n))
    )
  }
}
