package scalaz

import std.AllInstances._
import std.option.{some, none}
import Property.forAll

object MonadPlusTest extends Scalaprops {

  val unite = forAll{
    MonadPlus[List].unite(List(some(1), none[Int], some(2))) must_===(List(1, 2))
  }

  val uniteU = forAll{
    MonadPlus[List].uniteU(List(\/.right(1), \/.left("a"), \/.right(2))) must_===(List(1, 2))
  }

  val separate = forAll{
    import \&/._
    import syntax.monadPlus._

    List(\/.right(1), \/.left("a"), \/.right(2)).separate must_===(List("a") -> List(1, 2))

    List(1 -> "a", 2 -> "b").separate must_===(List(1, 2) -> List("a", "b"))

    Vector[Int \&/ String](This(1), Both(3, "a"), That("b")).separate must_===(Vector(1, 3) -> Vector("a", "b"))

    Stream(Success(1), Failure("a"), Success(2)).separate must_===(Stream("a") -> Stream(1, 2))
  }

  val filter = forAll{
    MonadPlus[List].filter(List(1, 2, 3))(_ % 2 == 0) must_===(List(2))
  }
}
