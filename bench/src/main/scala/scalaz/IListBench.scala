package scalaz

import org.openjdk.jmh.annotations.Benchmark
import scalaz.std.anyVal._
import scala.util.Random

object IListBench {
  val list: IList[Int] = IList.fromList(Random.shuffle(0 to 100000).toList)
}

class IListBench {

  @Benchmark def sortedNew() =
    IListBench.list.sorted

  @Benchmark def sortedOld() =
    IListBench.list.sortedOld

}

