package scalaz

import std.AllInstances._
import Property.forAll

object DisjunctionTest extends Scalaprops {

  val testLaws = Properties.list(
    laws.order.all[Int \/ Int],
    laws.monoid.all[Int \/ Int],
    laws.bindRec.all[Int \/ ?],
    laws.monad.all[Int \/ ?],
    laws.monadError.all[Int \/ ?, Int],
    laws.plus.all[Int \/ ?],
    laws.traverse.all[Int \/ ?]
  )

  val bitraverse = laws.bitraverse.all[\/]
  val associative = laws.associative.all[\/]

  val fromTryCatchThrowable = forAll {
    class Foo extends Throwable
    final class Bar extends Foo
    val foo = new Foo
    val bar = new Bar

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]
    implicit val equalBar = Equal.equalA[Bar]
    implicit val showBar = Show.showA[Bar]

    \/.fromTryCatchThrowable[Int, Foo](1) must_=== \/.right(1)
    \/.fromTryCatchThrowable[Int, Foo](throw foo) must_=== \/.left(foo)
    \/.fromTryCatchThrowable[Int, Foo](throw bar) must_=== \/.left(bar)
    \/.fromTryCatchThrowable[Int, Bar](throw foo).mustThrowA[Foo]
  }

  val recover = forAll {
    sealed trait Foo
    case object Bar extends Foo
    case object Baz extends Foo

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]

    -\/[Foo](Bar).recover({ case Bar => 1 }) must_=== \/-(1)
    -\/[Foo](Bar).recover({ case Baz => 1 }) must_=== -\/(Bar)
    \/.right[Foo, Int](1).recover({ case Bar => 4 }) must_=== \/-(1)
  }

  val recoverWith = forAll {
    sealed trait Foo
    case object Bar extends Foo
    case object Baz extends Foo

    implicit val equalFoo = Equal.equalA[Foo]
    implicit val showFoo = Show.showA[Foo]

    val barToBaz: PartialFunction[Foo, \/[Foo, Int]] = {
      case Bar => -\/(Baz)
    }

    val bazToInt: PartialFunction[Foo, \/[Foo, Int]] = {
      case Baz => \/-(1)
    }

    -\/[Foo](Bar).recoverWith(barToBaz) must_=== -\/(Baz)
    -\/[Foo](Bar).recoverWith(bazToInt) must_=== -\/(Bar)
    \/.right[Foo, Int](1).recoverWith(barToBaz) must_=== \/-(1)
  }

  val validation = forAll {
    import syntax.either._
    import syntax.validation._

    3.right[String].validation must_=== 3.success[String]
    "Hello".left[Int].validation must_=== "Hello".failure[Int]
  }

  val validationNel = forAll {
    import syntax.either._
    import syntax.validation._
    import syntax.apply._

    3.right[String].validationNel must_=== 3.successNel[String]
    ("hello".left[Int].validationNel |@| "world".left[Int].validationNel).tupled must_===
      ("hello".failureNel[Int] |@| "world".failureNel[Int]).tupled
  }
}
