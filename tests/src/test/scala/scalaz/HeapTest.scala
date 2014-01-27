package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.std.AllInstances._
import org.scalacheck.Prop.forAll

object HeapTest extends SpecLite {
  checkAll(equal.laws[Heap[Int]])
  checkAll(monoid.laws[Heap[Int]])
  checkAll(foldable.laws[Heap])

  def pred(i: Int) = i % 2 == 0

  implicit def headShow[A: Show]: Show[Heap[A]] =
    Contravariant[Show].contramap(Show[List[A]])(_.toList)

  "order maintained for toList" ! forAll {
    (a: Heap[Int]) => a.toList must_===(a.toList.sorted)
  }

  "toList / toStream" ! forAll {
    (a: Heap[Int]) => a.toStream must_===(a.toList.toStream)
  }

  "filter" ! forAll {
    (a: Heap[Int]) => a.filter(pred).toStream must_===(a.toStream.filter(pred))
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

  "size" ! forAll { a: Heap[Int] =>
    a.size must_=== a.toList.size
  }

  "minimum" ! forAll { a: Heap[Int] =>
    if(a.isEmpty){
      a.minimum.mustThrowA[RuntimeException]
    }else{
      a.minimum must_=== a.toList.min
    }
  }

  "map" ! forAll { a: Heap[Int] =>
    val f = (_: Int) + 1
    a.map(f).toList must_=== a.toList.map(f).sorted
  }

  "union" ! forAll { (a: Heap[Int], b: Heap[Int]) =>
    a.union(b).toList must_=== (a.toList ::: b.toList).sorted
  }

  "traverse" ! forAll { a: Heap[Int] =>
    a.traverse(Option.apply) must_=== Option(a)
    val f = (_: Int) + 1
    a.traverse[Id.Id, Int](f) must_=== a.map(f)
  }
}
