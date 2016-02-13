package scalaz

import std.AllInstances._
import syntax.foldable1._
import Property.forAll

object Foldable1Test extends Scalaprops {
  val maximum1 = forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.maximum1) must_===(xs.list.toList.max)
  }
  val maximumOf1 = forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs maximumOf1 f) must_===((xs.list.toList.iterator map f).max)
  }
  val maximumBy1 = forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs maximumBy1 f) must_===((xs.list zip (xs.list map f)).toList.maxBy(_._2)._1)
  }
  val minimum1 = forAll {
    (xs: NonEmptyList[Int]) =>
      (xs.minimum1) must_===(xs.list.toList.min)
  }
  val minimumOf1 = forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => Double = 1D + _
      (xs minimumOf1 f) must_===((xs.list.toList.iterator map f).min)
  }
  val minimumBy1 = forAll {
    (xs: NonEmptyList[Int]) =>
      val f: Int => String = _.toString
      (xs minimumBy1 f) must_===((xs.list zip (xs.list map f)).toList.minBy(_._2)._1)
  }

  val distinct1 = forAll {
    (xs: NonEmptyList[Int]) =>
      xs.distinct1.toList must_== xs.toList.distinct
      xs.distinct1(Order.order((_,_) => Ordering.EQ)).length must_== 1
  }

  val distinctE1 = forAll {
    (xs: NonEmptyList[Int]) =>
      xs.distinctE1.toList must_== xs.toList.distinct
      xs.distinctE1(Equal.equal((_,_) => true)).length must_== 1
  }

  private val L = Foldable1[NonEmptyList]

  val `product foldRight1 equivalence` = forAll {
    (l: NonEmptyList[IList[Int]], l2: NonEmptyList[IList[Int]]) =>
      (L.product(L).foldRight1((l, l2))(_ ++ _)
       must_===((l.list ++ l2.list).flatten))
  }

  val `product foldLeft1 equivalence` = forAll {
    (l: NonEmptyList[IList[Int]], l2: NonEmptyList[IList[Int]]) =>
      (L.product(L).foldLeft1((l, l2))((xs, x) => x ++ xs)
       must_===((l.list ++ l2.list).reverse.flatten))
  }

  val intercalate1 = forAll {
    (l: NonEmptyList[IList[Int]], x: IList[Int]) =>
      l.intercalate1(x) must_=== Foldable[IList].intercalate(l.list, x)
  }

  val toNel = forAll {
    intAnd: OneAnd[IList, Int] =>
    intAnd.toNel must_=== NonEmptyList.nel(intAnd.head, intAnd.tail)
  }
}
