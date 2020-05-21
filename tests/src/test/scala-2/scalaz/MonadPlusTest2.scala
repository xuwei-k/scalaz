package scalaz

import std.AllInstances._

class MonadPlusTest2 extends SpecLite {
  "uniteU" in {
    MonadPlus[List].uniteU(List[String \/ Int](\/.right(1), \/.left("a"), \/.right(2))) must_===(List(1, 2))
  }
}
