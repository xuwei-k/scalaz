package scalaz

import std.AllInstances._
import syntax.equal._
import Property.forAll

object DListTest extends Scalaprops {

  val testLaws = Properties.list(
    laws.equal.all[DList[Int]],
    laws.monoid.all[DList[Int]],
    laws.zip.all[DList],
    laws.traverse.all[DList],
    laws.isEmpty.all[DList],
    laws.bindRec.all[DList],
    laws.monadPlusStrong.all[DList]
  )

  val `DList append` = forAll {
    ((0 to 100000).foldLeft(DList[Int]())(_ :+ _).toList must_== (0 to 100000).toList)
  }

  val `headOption, tailOption` = forAll{ (n: Int, d: DList[Int]) =>

    // Defined when appropriate?
    val nonempty = d.uncons(false, (_, _) => true)
    d.headOption.isDefined must_=== nonempty
    d.tailOption.isDefined must_=== nonempty

    // If defined, are values correct?
    val d0 = n +: d
    assert(d0.headOption === Some(n)) // no Show instance, can't use must_===
    assert(d0.tailOption === Some(d))
    
  }

  object instances {
    def equal[A: Equal] = Equal[DList[A]]
    def monoid[A] = Monoid[DList[A]]
    def monadPlus = MonadPlus[DList]
    def bindrec = BindRec[DList]
    def traverse = Traverse[DList]
    def zip = Zip[DList]
    def isEmpty = IsEmpty[DList]
  }
}
