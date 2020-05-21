package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._
import org.scalacheck.Prop.forAll

class HeapTest extends SpecLite {
  checkAll(equal.laws[Heap[Int]])
  checkAll(monoid.laws[Heap[Int]])
  checkAll(foldable.laws[Heap])

  def pred(i: Int) = i % 2 == 0

  "order maintained for toList" ! forAll {
    (a: Heap[Int]) => a.toList must_===(a.toList.sorted)
  }

  "toList / toLazyList" ! forAll {
    (a: Heap[Int]) => a.toLazyList must_===(a.toList.to(LazyList))
  }

  "filter" ! forAll {
    (a: Heap[Int]) => a.filter(pred).toLazyList must_===(a.toLazyList.filter(pred))
  }

  "partition" ! forAll {
    (a: Heap[Int]) =>
      val (ts, fs) = a.partition(pred)
      ts.forall(pred) must_===(true)
      fs.exists(pred) must_===(false)
  }

  "split" ! forAll {
    (a: Heap[Int], x: Int) =>
      val (lt, eq, gt) = a.split(x)
      lt.forall(_ < x) must_===(true)
      eq.forall(_ == x) must_===(true)
      gt.forall(_ > x) must_===(true)
  }
}
