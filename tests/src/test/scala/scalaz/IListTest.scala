package scalaz

import std.AllInstances._
import syntax.bifunctor._, syntax.foldable._
import scalaz.Property.forAll

object IListTest extends Scalaprops {

  private implicit val stringGen = Tag.unsubst(Gen[String @@ GenTags.AlphaNum])

  val testLaws = Properties.list(
    laws.order.all[IList[Byte]],
    laws.monoid.all[IList[Byte]],
    laws.monadPlusStrong.all[IList],
    laws.traverse.all[IList],
    laws.cobind.all[IList],
    laws.isEmpty.all[IList],
    laws.align.all[IList],
    laws.zip.all[IList],
    laws.foldable.anyAndAllLazy[IList]
  )

  val bindRec = laws.bindRec.laws[IList].andThenParam(Param.maxSize(1))

  val `intercalate empty list is flatten` = forAll { (a: IList[IList[Int]]) =>
    a.intercalate(IList[Int]()) must_=== a.flatten
  }

  val `intersperse then remove odd items is identity` = forAll { (a: IList[Int], b: Int) =>
    val isEven = (_: Int) % 2 == 0
    a.intersperse(b).zipWithIndex.filter(p => isEven(p._2)).map(_._1) must_===(a)
  }

  val `intercalate is same as a.intersperse(b).flatten` = forAll { (a: IList[IList[Int]], b: IList[Int]) =>
    a.intercalate(b) must_===(a.intersperse(b).flatten)
  }

  val `intersperse vs benchmark` = forAll { (a: IList[Int], b: Int) =>
    def intersperse[A](value: IList[A], a: A): IList[A] = value match {
      case INil() => INil()
      case ICons(x, INil()) => x :: INil()
      case ICons(h, t) => h :: a :: intersperse(t, a)
    }
    a.intersperse(b) must_=== intersperse(a, b)
  }

  val `foldl is foldLeft` = forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    rnge.foldLeft(IList[Int]())(_++_) must_=== F.foldLeft(rnge.toList, IList[Int]())(_++_)
  }

  val `foldr is foldRight` = forAll {(rnge: IList[IList[Int]]) =>
    val F = Foldable[List]
    rnge.foldRight(IList[Int]())(_++_) must_=== F.foldRight(rnge.toList, IList[Int]())(_++_)
  }

  val `foldLeft1Opt` = forAll { ns: IList[List[Int]] =>
    ns.foldLeft1Opt(_ ::: _) must_=== ns.toList.reduceLeftOption(_ ::: _)
  }

  val `foldRight1Opt` = forAll { ns: IList[List[Int]] =>
    ns.foldRight1Opt(_ ::: _) must_=== ns.toList.reduceRightOption(_ ::: _)
  }

  val `foldMap1Opt` = forAll { ns: IList[List[Int]] =>
    ns.foldMap1Opt(identity) must_=== ns.toList.reduceLeftOption(_ ::: _)
  }

  val `mapAccumLeft` = forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumLeft(IList[Int]())((c, a) => (c :+ a, f(a))) must_=== (xs, xs.map(f))
  }

  val `mapAccumRight` = forAll { xs: IList[Int] =>
    val f = (_: Int) + 1
    xs.mapAccumRight(IList[Int]())((c, a) => (c :+ a, f(a))) must_=== (xs.reverse, xs.map(f))
  }

  // And some other tests that List doesn't have

  val `catamorphism` = forAll { (ns: IList[Int]) =>
    ns.foldRight(IList.empty[Int])(ICons(_, _)) must_=== ns
  }

  // Functionality borrowed from List is tested in terms of List. Is this ethical?
  // Should they be collapsed into fewer cases?

  val `++` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList must_=== ns.toList ++ ms.toList
  }

  val `++:` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++: ms).toList must_=== ns.toList ++: ms.toList
  }

  val `+:` = forAll { (n: Int, ns: IList[Int]) =>
    (n +: ns).toList must_=== n +: ns.toList
  }

  val `/:` = forAll { (ns: IList[Int], s: String, f: (String, Int) => String) =>
    (s /: ns)(f) == (s /: ns.toList)(f)
  }

  val `:+` = forAll { (n: Int, ns: IList[Int]) =>
    (ns :+ n).toList must_=== ns.toList :+ n
  }

  val `::` = forAll { (n: Int, ns: IList[Int]) =>
    (n :: ns).toList must_=== n :: ns.toList
  }

  val `:::` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ::: ms).toList must_=== ns.toList ::: ms.toList
  }

  val `:\\` = forAll { (ns: IList[Int], s: String, f: (Int, String) => String) =>
    (ns :\ s)(f) == (ns.toList :\ s)(f)
  }

  val concat = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns concat ms).toList must_=== ns.toList ++ ms.toList
  }

  val `collect` = forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collect(pf).toList must_=== ns.toList.collect(pf)
  }

  val `collectFirst` = forAll { (ns: IList[Int]) =>
    val pf: PartialFunction[Int, Int] = { case n if n % 2 == 0 => n + 1 }
    ns.collectFirst(pf) must_=== ns.toList.collectFirst(pf)
  }

  val plusPlus = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns ++ ms).toList must_=== ns.toList ++ ms.toList
  }

  val `containsSlice` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.containsSlice(ms) must_=== ns.toList.containsSlice(ms.toList)
  }

  val `count` = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.count(p) must_=== ns.toList.count(p)
  }

  val `distinct` = forAll { xs: IList[Int] =>
    xs.distinct.toList must_=== xs.toList.distinct
  }

  val `drop` = forAll { (ns: IList[Int], n: Int) =>
    ns.drop(n).toList must_=== ns.toList.drop(n)
  }

  val `dropRight` = forAll { (ns: IList[Int], n: Int) =>
    ns.dropRight(n).toList must_=== ns.toList.dropRight(n)
  }

  val `dropRightWhile` = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropRightWhile(p).toList must_=== ns.toList.reverse.dropWhile(p).reverse
  }

  val `dropWhile` = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.dropWhile(p).toList must_=== ns.toList.dropWhile(p)
  }

  val `endsWith` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.endsWith(ms) must_=== ns.toList.endsWith(ms.toList)
  }

  val `fill` = forAll { (a: Byte, b: Int) =>
    IList.fill(a)(b).toList must_=== List.fill(a)(b)
  }

  val `filter` = forAll { (ns: IList[Int], p: Int => Boolean) =>
    ns.filter(p).toList must_=== ns.toList.filter(p)
  }

  val `filterNot` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.filterNot(f).toList must_=== ns.toList.filterNot(f)
  }

  val `find` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.find(f) must_=== ns.toList.find(f)
  }

  // flatMap and folds are covered by laws

  val `groupBy` = forAll { (ns: IList[Int], f: Int => Int) =>
    ns.groupBy(f).map(_.toList).toList.toMap must_=== ns.toList.groupBy(f)
  }

  val `groupBy1` = forAll { (ns: IList[Int], f: Int => Int) =>
    ns.groupBy1(f).map(oa => (oa.head :: oa.tail).toList.reverse).toList.toMap must_=== ns.toList.groupBy(f)
  }

  val `headOption` = forAll { ns: IList[Int] =>
    ns.headOption must_=== ns.toList.headOption
  }

  val `index` = forAll { (ns: IList[Int], n: Int) =>
    ns.index(n) must_=== ns.toList.lift(n)
  }

  val `indexOf` = forAll { (ns: IList[Int], n: Int) =>
    ns.indexOf(n).getOrElse(-1) must_=== ns.toList.indexOf(n)
  }

  val `indexOfSlice` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.indexOfSlice(ms).getOrElse(-1) must_=== ns.toList.indexOfSlice(ms.toList)
  }

  val `indexWhere` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.indexWhere(f).getOrElse(-1) must_=== ns.toList.indexWhere(f)
  }

  val `initOption` = forAll { ns: IList[Int] =>
    ns.initOption.map(_.toList) must_=== (try Some(ns.toList.init) catch { case e: Exception => None })
  }

  val `inits` = forAll { ns: IList[Int] =>
    ns.inits.map(_.toList).toList must_=== ns.toList.inits.toList
  }

  val `interleave` = forAll { (xs: IList[Int], ys: IList[Int]) =>
    val a = xs interleave ys
    (xs.length + ys.length) must_=== a.length
    val min = math.min(xs.length, ys.length)

    Foldable[IList].all(xs.zipWithIndex){ case (x, i) =>
      val index = if(i <= min) i * 2 else (min * 2) + i - min
      a.index(index) == Some(x)
    } must_=== true

    Foldable[IList].all(ys.zipWithIndex){ case (y, i) =>
      val index = if(i < min) (i * 2) + 1 else (min * 2) + i - min
      a.index(index) == Some(y)
    } must_=== true

    xs.interleave(ys).toStream must_=== std.stream.interleave(xs.toStream, ys.toStream)
  }

  // intersperse is tested above
  // isEmpty is tested by empty laws

  val `lastIndexOf` = forAll { (ns: IList[Int], n: Int) =>
    ns.lastIndexOf(n).getOrElse(-1) must_=== ns.toList.lastIndexOf(n)
  }

  val `lastIndexOfSlice` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.lastIndexOfSlice(ms).getOrElse(-1) must_=== ns.toList.lastIndexOfSlice(ms.toList)
  }

  val `lastIndexWhere` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.lastIndexWhere(f).getOrElse(-1) must_=== ns.toList.lastIndexWhere(f)
  }

  val `lastOption` = forAll { ns: IList[Int] =>
    ns.lastOption must_=== ns.toList.lastOption
  }

  val `length` = forAll { ns: IList[Int] =>
    ns.length must_=== ns.toList.length
  }

  // map is tested by functor laws

  val `nonEmpty` = forAll { ns: IList[Int] =>
    ns.nonEmpty must_=== ns.toList.nonEmpty
  }

  val `padTo` = forAll { (ns: IList[Int], n: Int) =>
    ns.padTo(100, n).toList must_=== ns.toList.padTo(100, n)
  }

  val `patch` = forAll { (ns: IList[Int], a: Int, ms: IList[Int], b: Int) =>
    ns.patch(a, ms, b).toList must_=== ns.toList.patch(a, ms.toList, b)
  }

  val `prefixLength` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.prefixLength(f) must_=== ns.toList.prefixLength(f)
  }

  val `reduceLeftOption` = forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceLeftOption(f) must_=== (try Some(ns.toList.reduceLeft(f)) catch { case e:Exception => None })
  }

  val `reduceRightOption` = forAll { (ns: IList[Int], f: (Int, Int) => Int) =>
    ns.reduceRightOption(f) must_=== (try Some(ns.toList.reduceRight(f)) catch { case e:Exception => None })
  }

  val `reverse` = forAll { ns: IList[Int] =>
    ns.reverse.toList must_=== ns.toList.reverse
  }

  val `reverseMap` = forAll { (ns: IList[Int], f: Int => Int) =>
    ns.reverseMap(f).toList must_=== ns.toList.reverseMap(f)
  }

  val `reverse_:::` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    (ns reverse_::: ms).toList must_=== (ns.toList reverse_::: ms.toList)
  }

  val `scanLeft` = forAll { (ss: IList[String], f: (Int, String) => Int) =>
    ss.scanLeft(0)(f).toList must_=== ss.toList.scanLeft(0)(f)
    ss.scanLeft("z")(_ + _).toList must_=== ss.toList.scanLeft("z")(_ + _)
    ss.scanLeft(IList.empty[String])(_ :+ _).toList must_=== ss.toList.scanLeft(IList.empty[String])(_ :+ _)
  }

  val `scanRight` = forAll { (ss: IList[String], f: (String, Int) => Int)  =>
    ss.scanRight(0)(f).toList must_=== ss.toList.scanRight(0)(f)
    ss.scanRight("z")(_ + _).toList must_=== ss.toList.scanRight("z")(_ + _)
    ss.scanRight(IList.empty[String])(_ +: _).toList must_=== ss.toList.scanRight(IList.empty[String])(_ +: _)
  }

  val `slice` = forAll { (ns: IList[Int], a: Int, b: Int) =>
    ns.slice(a, b).toList must_=== ns.toList.slice(a, b)
  }

  val `sortBy` = forAll { (ss: IList[String], f: String => Int) =>
    ss.sortBy(f).toList must_=== ss.toList.sortBy(f)
  }

  val `sorted` = forAll { (ss: IList[String]) =>
    ss.sorted.toList must_=== ss.toList.sorted
  }

  val `span` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.span(f).umap(_.toList) must_=== ns.toList.span(f)
  }

  val `splitAt` = forAll { (ns: IList[Int], n: Int) =>
    ns.splitAt(n).umap(_.toList) must_=== ns.toList.splitAt(n)
  }

  val `startsWith` = forAll { (ns: IList[Int], ms: IList[Int]) =>
    ns.startsWith(ms) must_=== ns.toList.startsWith(ms.toList)
  }

  val `tails` = forAll { ns: IList[Int] =>
    ns.tails.map(_.toList).toList must_=== ns.toList.tails.toList
  }

  val `tailOption` = forAll { ns: IList[Int] =>
    ns.tailOption.map(_.toList) must_=== (try Some(ns.toList.tail) catch { case e: Exception => None })
  }

  val `take` = forAll { (ns: IList[Int], n: Byte) =>
    ns.take(n).toList must_=== ns.toList.take(n)
  }

  val `takeRight` = forAll { (ns: IList[Int], n: Byte) =>
    ns.takeRight(n).toList must_=== ns.toList.takeRight(n)
  }

  val `takeRightWhile` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeRightWhile(f).toList must_=== ns.toList.reverse.takeWhile(f).reverse
  }

  val `takeWhile` = forAll { (ns: IList[Int], f: Int => Boolean) =>
    ns.takeWhile(f).toList must_=== ns.toList.takeWhile(f)
  }

  val `toEphemeralStream` = forAll { ns: List[Int] =>
    IList(ns: _*).toEphemeralStream.toList must_=== EphemeralStream(ns: _*).toList
  }

  val `toList` = forAll { ns: List[Int] =>
    IList(ns: _*).toList must_=== ns
  }

  val `toMap` = forAll { ps: List[(String, Int)] =>
    IList(ps: _*).toMap must_=== ==>>(ps: _*)
  }

  val `toNel` = forAll { ns: List[Int] =>
    IList(ns: _*).toNel must_=== Scalaz.ToListOpsFromList(ns).toNel
  }

  val `toStream` = forAll { ns: List[Int] =>
    IList(ns: _*).toStream must_=== ns.toStream
  }

  val `toVector` = forAll { ns: Vector[Int] =>
    IList(ns: _*).toVector must_=== ns
  }

  val `toZipper` = forAll { ns: List[Int] =>
    IList(ns: _*).toZipper must_=== scalaz.std.stream.toZipper(ns.toStream)
  }

  // uncons is tested everywhere

  val `updated` = forAll { (ns: IList[Int], i: Int, n: Int) =>
    if (i < 0 || i >= ns.length) {
      ns.updated(i, n) must_=== ns
    } else {
      ns.updated(i, n).toList must_=== ns.toList.updated(i, n)
    }
  }

  val `unzip` = forAll { (ns: IList[(Int, String)]) =>
    ns.unzip.bimap(_.toList, _.toList) must_=== ns.toList.unzip
  }

  // widen is tested by toMap and unzip
  // zip is tested by zip laws

  val `zipWithIndex` = forAll { ns: IList[Int] =>
    ns.zipWithIndex.toList must_=== ns.toList.zipWithIndex
  }

  object instances {
    def equal[A: Equal] = Equal[IList[A]]
    def order[A: Order] = Order[IList[A]]
    def monoid[A] = Monoid[IList[A]]
    def monadPlus = MonadPlus[IList]
    def bindrec = BindRec[IList]
    def traverse = Traverse[IList]
    def zip = Zip[IList]
    def align = Align[IList]
    def isEmpty = IsEmpty[IList]
    def cobind = Cobind[IList]
  }
}
