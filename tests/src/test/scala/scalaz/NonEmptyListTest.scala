package scalaz

import std.AllInstances._
import Property.forAll

object NonEmptyListTest extends Scalaprops {
  val testLaws = Properties.list(
    laws.monad.all[NonEmptyList],
    laws.bindRec.all[NonEmptyList],
    laws.plus.all[NonEmptyList],
    laws.semigroup.all[NonEmptyList[Int]],
    laws.equal.all[NonEmptyList[Int]],
    laws.order.all[NonEmptyList[Int]],
    laws.traverse1.all[NonEmptyList],
    laws.foldable.anyAndAllLazy[NonEmptyList],
    laws.zip.all[NonEmptyList],
    laws.align.all[NonEmptyList],
    laws.comonad.all[NonEmptyList]
  )

  val scanLeft1 = forAll { fa: NonEmptyList[List[Int]] =>
    def f[A]: (List[A], List[A]) => List[A] = _ ::: _
    val a = Foldable1[NonEmptyList].scanLeft1(fa)(f)
    a.list must_=== fa.tail.scanLeft(fa.head)(f)
    a.size must_=== fa.size
  }

  val scanRight1 = forAll { fa: NonEmptyList[List[Int]] =>
    def f[A]: (List[A], List[A]) => List[A] = _ ::: _
    val a = Foldable1[NonEmptyList].scanRight1(fa)(f)
    a.list must_=== fa.init.scanRight(fa.last)(f)
    a.size must_=== fa.size
  }

  val `findLeft/findRight` = forAll {
    val a = NonEmptyList(1, 2, 3, 4, 5)
    Foldable[NonEmptyList].findLeft(a)(_ % 2 == 0) must_=== Some(2)
    Foldable[NonEmptyList].findRight(a)(_ % 2 == 0) must_=== Some(4)
  }

  val findLeft = forAll{ a: NonEmptyList[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[NonEmptyList].findLeft(a)(f) must_=== Foldable[IList].findLeft(a.list)(f)
  }

  val findRight = forAll { a: NonEmptyList[Int] =>
    val f = (_: Int) % 3 == 0
    Foldable[NonEmptyList].findRight(a)(f) must_=== Foldable[IList].findRight(a.list)(f)
  }

  val distinct = forAll { xs: NonEmptyList[Int] =>
    Option(xs.distinct) must_=== std.list.toNel(Foldable[NonEmptyList].toList(xs).distinct)
  }

  val `NonEmptyList size is correct` = forAll { xs:NonEmptyList[Int] =>
    xs.size must_===(1 + xs.tail.count(b => true)) 
  }

  val `foldl1 is reduceLeft` = forAll {(rnge: NonEmptyList[IList[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.toList.reduceLeft(_++_) must_===(F.foldl1(rnge)(a => b => a ++ b))
  }

  val `foldr1 is reduceRight` = forAll {(rnge: NonEmptyList[IList[Int]]) =>
    val F = Foldable1[NonEmptyList]
    rnge.list.toList.reduceRight(_++_) must_===(F.foldr1(rnge)(a => b => a ++ b))
  }
  val `foldRight1 is reduceRight` = forAll { xs: NonEmptyList[IList[Int]] =>
    val F = Foldable1[NonEmptyList]
    xs.list.toList.reduceRight(_ ++ _) must_== F.foldRight1(xs)(_ ++ _)
  }
  val `NonEmptyList.last is correct` = forAll { xs:NonEmptyList[Int] =>
    xs.reverse.head must_===(xs.last)
  }
  val `NonEmptyList.init size is correct` = forAll { xs:NonEmptyList[Int] =>
    xs.init.count(a => true) must_===(xs.tail.count(a => true))
  }
  val `correctness of tails` = forAll { xs: NonEmptyList[Int] =>
    import NonEmptyList._
    xs.tails must_=== nel(xs, xs.tail match {
      case INil() => INil()
      case ICons(h, t) => nel(h, t).tails.list
    })
  }
  val `toNel is self` = forAll { xs: NonEmptyList[Int] =>
    Foldable1[NonEmptyList].toNel(xs) must_=== xs
  }
  val zipWithIndex = forAll { xs: NonEmptyList[Int] =>
    xs.zipWithIndex.list must_== xs.list.zipWithIndex
  }
}
