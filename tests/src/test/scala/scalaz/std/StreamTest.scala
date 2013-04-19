package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

class StreamTest extends Spec {
  checkAll(equal.laws[Stream[Int]])
  checkAll(monoid.laws[Stream[Int]])
  checkAll(monadPlus.strongLaws[Stream])
  checkAll(traverse.laws[Stream])
  checkAll(isEmpty.laws[Stream])

  {
    import org.scalacheck._
    import scalaz.scalacheck.ScalaCheckBinding._

    type ZipStream[A] = Stream[A] @@ Tags.Zip
    implicit def zipStreamArbitrary[A](implicit a: Arbitrary[A]): Arbitrary[ZipStream[A]] =
      Functor[Arbitrary].map(implicitly[Arbitrary[Stream[A]]])(Tags.Zip)

    implicit def zipStreamEqual[A](implicit A0: Equal[A]) = new Equal[ZipStream[A]] {
      def equal(a1: ZipStream[A], a2: ZipStream[A]) = (a1 corresponds a2)(A0.equal)
    }

    checkAll(applicative.laws[ZipStream])
  }

  import std.stream.streamSyntax._
  import syntax.foldable._

  "intercalate empty stream is flatten" ! check((a: Stream[Stream[Int]]) => a.intercalate(Stream.empty[Int]) must be_===(a.flatten))

  "intersperse then remove odd items is identity" ! prop {
    (a: Stream[Int], b: Int) =>
      val isEven = (_: Int) % 2 == 0
      a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must be_===(a)
  }

  "intercalate is same as intersperse(s).flatten" ! check  {
    (a: Stream[Stream[Int]], b: Stream[Int]) =>
      a.intercalate(b) must be_===(a.intersperse(b).flatten)
  }

  "intersperse vs benchmark" ! check  {
    def intersperse[A](as: Stream[A], a: A): Stream[A] = {
      def loop(rest: Stream[A]): Stream[A] = rest match {
        case Stream.Empty => Stream.empty
        case h #:: t      => a #:: h #:: loop(t)
      }
      as match {
        case Stream.Empty => Stream.empty
        case h #:: t      => h #:: loop(t)
      }
    }
    (a: Stream[Int], b: Int) => (a.intersperse(b) must be_===(intersperse(a, b)))
  }


  "foldl is foldLeft" ! prop {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldLeft(List[Int]())(_++_)
      must be_===(F.foldLeft(rnge, List[Int]())(_++_)))
  }

  "foldr is foldRight" ! prop {(rnge: Stream[List[Int]]) =>
    val F = Foldable[Stream]
    (rnge.foldRight(List[Int]())(_++_)
      must be_===(F.foldRight(rnge, List[Int]())(_++_)))
  }
}
