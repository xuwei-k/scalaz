package scalaz

import std.AllInstances._

object TreeLocTest extends Scalaprops {
  import Property.{forAll, forAllG}

  val testLaws = Properties.list(
    laws.order.all[TreeLoc[Int]],
    laws.traverse1.all[TreeLoc],
    laws.foldable.anyAndAllLazy[TreeLoc]
  )

  val treeLocGenSized = forAllG(Gen.choose(1, 100), Gen[Long]){ (size, seed) =>
    Gen[TreeLoc[Unit]].samples(
      listSize = 10,
      seed = seed,
      size = size
    ).map(Foldable[TreeLoc].length(_)).forall(_ == size)
  }.toProperties((), Param.minSuccessful(10))

  val comonad = {
    def treeEqual[A: Equal]: Equal[Tree[A]] = new Equal[Tree[A]] {
      import std.stream.streamEqual
      def streamEqualApprox = streamEqual[Tree[A]].contramap((_: Stream[Tree[A]]).take(1000))
      def equal(a1: Tree[A], a2: Tree[A]) =
        Equal[A].equal(a1.rootLabel, a2.rootLabel) && streamEqualApprox.equal(a1.subForest, a2.subForest)
    }

    laws.comonad.all[TreeLoc]
  }

  val `TreeLoc from empty forest does not throw an exception` = forAll{
    import scalaz.std.option._
    val result: Option[TreeLoc[Int]] = TreeLoc.fromForest(Stream.empty[Tree[Int]])
    result must_==(none[TreeLoc[Int]])
  }

  object instances {
    def equal[A: Equal] = Equal[TreeLoc[A]]
    def order[A: Order] = Order[TreeLoc[A]]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[TreeLoc[A]]
  }
}
