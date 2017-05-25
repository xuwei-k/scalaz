package scalaz

import scalaz.Scalaz._
import scalaz.iteratee.Iteratee._
import scalaz.effect._
import scalaz.Id.Id

object TestMain {
  def main(args: Array[String]): Unit = {
    val a = List(1, 2, 3)
    assert(a.corresponds(a)(_ == _))
  }
}
