package scalaz

import std.AllInstances._
import Tree._
import Property.forAll

object TreeTest extends Scalaprops {

  val testLaws = Properties.list(
    laws.order.all[Tree[Int]],
    laws.traverse1.all[Tree],
    laws.applicative.all[Tree],
    laws.comonad.all[Tree],
    laws.align.all[Tree],
    laws.zip.all[Tree],
    laws.foldable.anyAndAllLazy[Tree]
  )

  val indexed = forAll { xs: Tree[Byte] =>
    val F = Traverse[Tree]
    val a = F.indexed(xs)
    Equal[Tree[Byte]].equal(a.map(_._2), xs) must_=== true
    F.toList(a) must_=== F.toList(xs).zipWithIndex.map{case (a, b) => (b, a)}
  }

  val treeGenSized = Property.forAllG(Gen.positiveByte, Gen[Long]){ (n, seed) =>
    val size = 5
    val a = Gen.treeGenSized[Unit](n).samples(
      listSize = size, seed = seed
    ).map(Foldable[Tree].length)

    a must_=== List.fill(size)(n)
  }

  val `infinite Tree flatten` = forAll{
    Node(0, Stream.from(1).map(Leaf(_))).flatten
    true
  }

  val `deep Tree flatten should not cause a stack overflow` = forAll{
    val size = 1000000
    val tree = (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))
    tree.flatten must_== (size to 0 by -1).toStream
  }

  val `A tree must can be rendered as an ASCII string` = forAll{
    Node(1, Stream(Node(2, Stream(Leaf(3))), Leaf(4))).drawTree must_== Seq(
      "1",
      "|",
      "+- 2",
      "|  |",
      "|  `- 3",
      "|",
      "`- 4").mkString("", "\n", "\n")
  }
}
