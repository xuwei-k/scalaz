package scalaz

import scalaz.std.anyVal._
import scalaz.std.list._
import scalaz.std.java.enum._

object CofreeTest {

  implicitly[Equal[Cofree[List, Int]]]

}
