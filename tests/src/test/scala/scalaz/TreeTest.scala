package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Prop.forAll

object TreeTest extends SpecLite {

  private[this] def drawOld0[A](tree: Tree[A])(implicit sh: Show[A]): Stream[String] = {
    def drawSubTrees(s: Stream[Tree[A]]): Stream[String] = s match {
      case Stream.Empty => Stream.Empty
      case Stream(t)    => "|" #:: shift("`- ", "   ", drawOld0(t))
      case t #:: ts     => "|" #:: shift("+- ", "|  ", drawOld0(t)) append drawSubTrees(ts)
    }
    def shift(first: String, other: String, s: Stream[String]): Stream[String] =
      (first #:: Stream.continually(other)).zip(s).map {
        case (a, b) => a + b
      }

    sh.shows(tree.rootLabel) #:: drawSubTrees(tree.subForest)
  }

  private[this] def drawOld[A](tree: Tree[A])(implicit sh: Show[A]): String =
    drawOld0(tree).mkString("", "\n", "\n")

  def deepTree(n: Int) = (1 to n).foldLeft(Tree.leaf(0)){
    (a, b) => Tree.node(b, Stream(a))
  }

  def time[A](f: => A): (Double, A) = {
    val s = System.nanoTime
    val r = f
    val t = (System.nanoTime - s) / 1000000.0
    (t, r)
  }

  "deepTree draw time" ! {
    List(100, 200, 400, 800, 1000, 1200).foreach{ n =>
      val t1 = deepTree(n)
      val t2 = deepTree(n)
      val newTime = time(t1.drawTree)._1
      val oldTime = time(drawOld(t2))._1
      println((n, newTime, oldTime))
    }
    true
  }

  "consistent old implementation" ! forAll{ tree: Tree[Byte] =>
    val l = Foldable[Tree].length(tree)
    val (newTime, a) = time(tree.drawTree)
    val (oldTime, b) = time(drawOld(tree))
    println((l, newTime / oldTime))
    a must_=== b
  }

}
