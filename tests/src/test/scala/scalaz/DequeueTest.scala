package scalaz

import std.AllInstances._
import Property.forAll

object DequeueTest extends Scalaprops {

  val testLaws = Properties.list(
    laws.monoid.all[Dequeue[Int]],
    laws.isEmpty.all[Dequeue],
    laws.foldable.all[Dequeue],
    laws.plusEmpty.all[Dequeue],
    laws.functor.all[Dequeue]
  )

  val `fromList works` = forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toStream must_===(l.toStream)
  }

  val `reverse twice is id` = forAll{ (dq: Dequeue[Int]) ⇒
    dq.reverse.reverse.toIList.must_===(dq.toIList)
  }

  val `size consistent with source list` = forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).size must_===(l.size)
  }

  val `reverse stream works` = forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toBackStream must_===(l.reverse.toStream)
  }

  val `toIList consistent with Ilist.fromFoldable` = forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toIList must_===(IList.fromFoldable(l))
  }

  val `toBackIList consistent with Ilist.fromFoldable` = forAll{ (l: List[Int]) ⇒
    Dequeue.fromFoldable(l).toBackIList must_===(IList.fromFoldable(l.reverse))
  }

  val `snoc works` = forAll{ (l: List[Int]) ⇒
    (l.foldLeft[Dequeue[Int]](Dequeue.empty)((q,a) ⇒ q snoc a)).toStream must_=== l.toStream
  }

  val `cons works` = forAll{ (l: List[Int]) ⇒
    (l.foldRight[Dequeue[Int]](Dequeue.empty)((a,q) ⇒ q cons a)).toStream must_=== l.toStream
  }
}
