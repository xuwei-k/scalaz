package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import Id._
import org.scalacheck.Prop
import org.scalacheck.Prop.forAll

object ListTest extends SpecLite {
  checkAll(equal.laws[List[Int]])
  checkAll(monoid.laws[List[Int]])
  checkAll(monadPlus.strongLaws[List])
  checkAll(traverse.laws[List])
  checkAll(zip.laws[List])
  checkAll(align.laws[List])
  checkAll(isEmpty.laws[List])
  checkAll(cobind.laws[List])
  checkAll(order.laws[List[Int]])

  import std.list.listSyntax._
  import syntax.foldable._

  "intercalate empty list is flatten" ! forAll((a: List[List[Int]]) => a.intercalate(List[Int]()) must_===(a.flatten))

  "intersperse then remove odd items is identity" ! forAll {
    (a: List[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  "intercalate is same as a.intersperse(b).flatten" ! forAll {
    (a: List[List[Int]], b: List[Int]) =>
      a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! forAll {
    def intersperse[A](value: List[A], a: A): List[A] = value match {
      case Nil      => Nil
      case x :: Nil => x :: Nil
      case h :: t   => h :: a :: intersperse(t, a)
    }
    (a: List[Int], b: Int) => (a.intersperse(b) must_===(intersperse(a, b)))
  }

  "groupWhenM[Id].flatten is identity" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhenM[Id](p).flatten must_===(a)
  }

  "groupBy1" ! forAll {
      (a: List[String]) =>
      val strlen = (_ : String).length
      (a groupBy strlen) must_===((a groupBy1 strlen) mapValues (_.list))
  }

  "groupWhen.flatten is identity" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhen(p).flatten must_===(a)
  }

  "filterM" ! forAll {
    (xs: List[Int]) => xs.filterM[Id](_ % 2 == 0) == xs.filter(_ % 2 == 0)
  }

  "groupByWhen splits a list at each point where `p(as(n), as(n+1))` yields false" ! forAll {
    (a: List[Int], p: (Int, Int) => Boolean) =>
      a.groupWhen(p).forall { group: List[Int] =>
        (0 until group.size - 1).forall { i: Int => p(group(i), group(i+1)) }
      }
  }

  "takeWhileM example" in {
    def takeWhileN[A](as: List[A], n: Int)(f: A => Boolean): List[A] = as.takeWhileM[({type λ[α] = State[Int, α]})#λ](a => State {
      i =>
        val j = i + (if (f(a)) 0 else 1)
        val done = j >= n
        (j, !done)
    }).evalZero[Int]

    val actual = takeWhileN("/abc/def/hij/klm".toList, 4)(_ != '/').mkString
    actual must_===("/abc/def/hij")
  }

  "foldl is foldLeft" ! forAll {(rnge: List[List[Int]]) =>
    val F = Foldable[List]
    (rnge.foldLeft(List[Int]())(_++_)
      must_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! forAll {(rnge: List[List[Int]]) =>
    val F = Foldable[List]
    (rnge.foldRight(List[Int]())(_++_)
      must_===(F.foldRight(rnge, List[Int]())(_++_)))
  }

  "index" ! forAll { (xs: List[Int], n: Int) =>
    (xs index n) must_===(if (n >= 0 && xs.size > n) Some(xs(n)) else None)
  }

  "groupWhen is groupWhenM[Id]" ! forAll { xs: List[Int] =>
    val f: (Int, Int) => Boolean = _ > _
    xs.groupWhen(f) must_=== xs.groupWhenM[Id.Id](f)
  }

  "mapAccumLeft" ! forAll {
    (xs: List[Int]) =>
      val f = (_: Int) + 1
      xs.mapAccumLeft(List[Int](), (c: List[Int], a) =>
        (c :+ a, f(a))) must_===(xs, xs.map(f))
  }

  "mapAccumRight" ! forAll {
    (xs: List[Int]) =>
      val f = (_: Int) + 1
      xs.mapAccumRight(List[Int](), (c: List[Int], a) =>
        (c :+ a, f(a))) must_===(xs.reverse, xs.map(f))
  }

  "no stack overflow" should {
    import Id.Id

    def cons[A]: A => IList[A] = (_: A) :: IList.empty[A]

    val constTrue: Int => Id[Boolean]  = (_: Int) => true
    val constFalse: Int => Id[Boolean] = (_: Int) => false

    val constTrueList  = constTrue andThen cons
    val constFalseList = constFalse andThen cons

    import syntax.std.list._
    val a = scala.util.Random.shuffle((1 to 500000).toList)

    "takeWhileM" ! {
      a.takeWhileM(constTrue) must_=== a
      a.takeWhileM(constTrueList) must_=== cons(a)
    }
    "takeUntilM" ! {
      a.takeUntilM(constFalse) must_=== a
      a.takeUntilM(constFalseList) must_=== cons(a)
    }
    "filterM" ! {
      a.filterM(constTrue) must_=== a
      a.filterM(constTrueList) must_=== cons(a)
    }
    "findM" ! {
      a.findM(constFalse) must_=== None
      a.findM(constFalseList) must_=== cons(None)
    }
    "partitionM" ! {
      val f = (_: Int) % 3 == 0
      a.partitionM[Id](f) must_=== a.partition(f)
      a.partitionM(f andThen cons) must_=== cons(a.partition(f))
    }
    "spanM" ! {
      a.spanM(constTrue) must_=== a.span(constTrue)
      a.spanM(constTrueList) must_=== cons(a.span(constTrue))
    }
    "breakM" ! {
      a.breakM(constFalse) must_=== a.span(constTrue)
      a.breakM(constFalseList) must_=== cons(a.span(constTrue))
    }
    "groupWhenM" ! {
      import syntax.functor._
      val f = (_: Int) > ( _: Int) * 2
      a.groupWhenM[Id](f) must_=== a.groupWhen(f)
      a.groupWhenM(f map cons) must_=== cons(a.groupWhen(f))
    }
  }

}
