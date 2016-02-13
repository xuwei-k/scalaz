package scalaz

import std.AllInstances._
import scala.util.Random
import Property.forAll

object DievTest extends Scalaprops {
  val random = new Random()

  val `insert order makes no difference` = forAll {
    (list: List[Int]) => {
      val shuffledList = random.shuffle(list)
      val dievFromList = list.foldLeft(Diev.empty[Int])(_ + _)
      val dievFromShuffledList = shuffledList.foldLeft(Diev.empty[Int])(_ + _)
      dievFromList must_===(dievFromShuffledList)
    }
  }

  val fixIntervalOrder = forAll {
    (tuple: (Int, Int)) => {
      val expectedResult = if (tuple._1 > tuple._2) tuple.swap else tuple
      DievInterval.fixIntervalOrder(tuple) must_===(expectedResult)
    }
  }

  val `fromValuesSeq / toSet` = forAll {
    (set: Set[Int]) => Diev.fromValuesSeq(set.toSeq).toSet must_===(set)
  }

  val `fromValuesSeq / toList` = forAll {
    (list: List[Int]) => {
      val sortedList = list.toSet.toList.sorted
      Diev.fromValuesSeq(list).toList must_===(sortedList)
    }
  }

  val `++ associativity` = forAll {
    (first: Diev[Int], second: Diev[Int]) => first ++ second must_===(second ++ first)
  }

  val `intervals / fromIntervalsSeq` = forAll {
    (original: Diev[Int]) => Diev.fromIntervalsSeq(original.intervals) must_===(original)
  }

  val `-- / ++` = forAll {
    (first: Diev[Int], second: Diev[Int]) => first -- second ++ second must_===(first ++ second)
  }

  val testLaws = Properties.list(
    laws.equal.all[Diev[Int]],
    laws.monoid.all[Diev[Int]]
  )
}
