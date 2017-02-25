package scalaz

import Scalaz._

object Main {
  def main(args: Array[String]): Unit = {
    val list1 = List[Option[Int]](Some(1), Some(2), Some(3), Some(4))
    println(Traverse[List].sequence(list1))
  }
}
