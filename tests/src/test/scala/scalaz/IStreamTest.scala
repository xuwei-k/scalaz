package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.contravariant._
import syntax.foldable._
import org.scalacheck.Prop.forAll

object IStreamTest extends SpecLite {

  checkAll(monadPlus.strongLaws[IStream])
  checkAll(isEmpty.laws[IStream])
  checkAll(traverse.laws[IStream])
  checkAll(equal.laws[IStream[Int]])

  implicit def iStreamShow[A: Show]: Show[IStream[A]] =
    Show[List[A]].contramap(_.toList)

  "reverse" ! forAll{ (e: IStream[Int]) =>
    e.reverse.toList must_===(e.toList.reverse)
    e.reverse.reverse must_===(e)
  }

  "Foldable.foldLeft" ! forAll{ (xs: List[List[Int]]) =>
    Foldable[IStream].foldLeft(IStream.fromFoldable(xs), List[Int]())(_ ::: _) must_===(xs.foldLeft(List[Int]())(_ ::: _))
  }

  "foldMap evaluates lazily" in {
    Foldable[IStream].foldMap(IStream.Lazy.infinite(false))(identity)(booleanInstance.conjunction) must_===(false)
  }

  // https://github.com/scalaz/scalaz/issues/1515
  "traverse is curiously lazy over Id" ! {
    import syntax.traverse._
    import Scalaz.Id

    val stream = IStream.Lazy.infinite(1)
    val _ = stream.traverse[Id, Int](identity)
    // got here without exception... that's good!
  }

  "length" in {
    var counter = 0

    def a(): Unit = {
      counter += 1
      42
    }

    val x = IStream.ByName.cons(
      a(),
      IStream.ByName.cons(
        a(),
        IStream.ByName.cons(
          a(),
          IStream.ByName(a())
        )
      )
    )
    Foldable[IStream].length(x) must_=== 4
    counter must_=== 0
  }

  "take from infinite LazyList" in {
    val n = util.Random.nextInt(1000)
    val list = LazyList.iterate(0)(_ + 1).take(n)
    list must_=== Foldable[IStream].toLazyList(IStream.fromLazyList(LazyList.iterate(0)(_ + 1).take(n)))
  }

  "index infinite LazyList" in {
    val i = util.Random.nextInt(1000)
    val xs = LazyList from 0
    Foldable[IStream].index(IStream.fromLazyList(xs), i) must_===(xs.lift.apply(i))
  }

  "foldMap evaluates lazily" in {
    val infiniteStream = IStream.fromLazyList(LazyList.iterate(false)(identity))
    Foldable[IStream].foldMap(infiniteStream)(identity)(booleanInstance.conjunction) must_===(false)
  }

  "foldMap1Opt identity" ! forAll {
    (xs: IStream[Int]) =>
    Foldable[IStream].foldMap1Opt(xs)(Vector(_)).getOrElse(Vector.empty) must_===(Foldable[IStream].toVector(xs))
  }

  /* TODO
  "foldMap1Opt evaluates lazily" in {
    val infiniteStream = IStream.fromLazyList(LazyList.iterate(false)(identity))
    Foldable[IStream].foldMap1Opt(infiniteStream)(identity)(booleanInstance.conjunction) must_===(Some(false))
  }
  */

  "foldRight evaluates lazily" in {
    val infiniteStream = IStream.fromLazyList(LazyList.iterate(true)(identity))
    Foldable[IStream].foldRight(infiniteStream, true)(_ || _) must_===(true)
  }

  "foldMapLeft1Opt identity" ! forAll {
    (xs: IStream[Int]) =>
    Foldable[IStream].foldMapLeft1Opt(xs.reverse)(IStream.Strict(_))((xs, x) => IStream.Strict.cons(x, xs)) must_===(
      if (IsEmpty[IStream].isEmpty(xs)) None else Some(xs)
    )
  }

  "foldMapRight1Opt identity" ! forAll {
    (xs: IStream[Int]) =>
    Foldable[IStream].foldMapRight1Opt(xs)(IStream.Strict(_))((x, xs) => IStream.Lazy.cons(x, xs)) must_===(
      if (IsEmpty[IStream].isEmpty(xs)) None else Some(xs)
    )
  }

  /* TODO
  "foldMapRight1Opt evaluates lazily" in {
    val infiniteStream = IStream.fromLazyList(LazyList.iterate(true)(identity))
    Foldable[IStream].foldMapRight1Opt(infiniteStream)(identity)(_ || _) must_===(Some(true))
  }
  */

}
