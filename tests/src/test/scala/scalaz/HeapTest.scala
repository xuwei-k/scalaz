package scalaz

import scalaz.std.AllInstances._
import Property.forAll

object HeapTest extends Scalaprops {
  val equal = laws.equal.all[Heap[Int]]
  val monoid = laws.monoid.all[Heap[Int]]
  val foldable = laws.foldable.all[Heap]

  val `order maintained for toList` = forAll {
    (a: Heap[Int]) => a.toList must_===(a.toList.sorted)
  }

  val `toList / toStream` = forAll {
    (a: Heap[Int]) => a.toStream must_===(a.toList.toStream)
  }

  val filter = forAll {
    (a: Heap[Int], pred: Int => Boolean) =>
      a.filter(pred).toStream must_===(a.toStream.filter(pred))
  }

  val partition = forAll {
    (a: Heap[Int], pred: Int => Boolean) =>
      val (ts, fs) = a.partition(pred)
      ts.forall(pred) must_===(true)
      fs.exists(pred) must_===(false)
  }

  val split = forAll {
    (a: Heap[Int], x: Int) =>
      val (lt, eq, gt) = a.split(x)
      lt.forall(_ < x) must_===(true)
      eq.forall(_ == x) must_===(true)
      gt.forall(_ > x) must_===(true)
  }
}
