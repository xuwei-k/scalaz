package scalaz

import std.AllInstances._
import Property.forAll

object LazyOptionTest extends Scalaprops {
  val testLaws = Properties.list(
    laws.equal.all[LazyOption[Int]],
    laws.bindRec.all[LazyOption],
    laws.monadPlusStrong.all[LazyOption],
    laws.cobind.all[LazyOption],
    laws.traverse.all[LazyOption],
    laws.zip.all[LazyOption],
    laws.align.all[LazyOption],
    laws.isEmpty.all[LazyOption]
  )

  val monoidLaws = laws.monoid.all[LazyOption[Int]]

  val monoid = forAll { (a: LazyOption[Int], b: LazyOption[Int]) =>
    Monoid[LazyOption[Int]].append(a, b).toOption must_=== Monoid[Option[Int]].append(a.toOption, b.toOption)
  }

  val `tail recursive tailrecM` = forAll {
    val times = 10000
    
    val result = 
      BindRec[LazyOption].tailrecM[Int, Int] { 
        i => LazyOption.lazySome(if (i < 10000) \/.left(i + 1) else \/.right(i)) 
      }(0)
    result.getOrElse(0) must_=== times
  }
  
  object instances {
    def equal[A: Equal] = Equal[LazyOption[A]]
    def monadPlus = MonadPlus[LazyOption]
    def bindrec = BindRec[LazyOption]
    def cobind = Cobind[LazyOption]
    def traverse = Traverse[LazyOption]
    def zip = Zip[LazyOption]
    def align = Align[LazyOption]
    def isEmpty = IsEmpty[LazyOption]
    def monoid[A: Semigroup] = Monoid[LazyOption[A]]
  }
}
