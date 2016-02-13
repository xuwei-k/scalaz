package scalaz

import std.AllInstances._
import Property.forAll

object ListTTest extends Scalaprops {
  type ListTOpt[A] = ListT[Option, A]

  val `fromList / toList` = forAll {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).toList must_===(ass)
  }

  val `filter all` = forAll {
    (ass: ListT[List, Int]) =>
      ass.filter(_ => true) must_===(ass)
  }

  val `filter none` = forAll {
    (ass: ListT[List, Int]) =>
      val filtered = ass.filter(_ => false)
      val isEmpty = filtered.isEmpty
      isEmpty.toList.forall(identity)
  }

  val find = forAll {
    (ass: ListTOpt[Int]) =>
      ass.find(_ > 0 ) must_===(OptionT.optionT(ass.run.map(_.find( _ > 0))))
  }
  
  val drop = forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).drop(x).toList must_===(ass.map(_.drop(x)))
  }
  
  val take = forAll {
    (ass: Option[List[Int]], x: Int) =>
      ListT.fromList(ass).take(x).toList must_===(ass.map(_.take(x)))
  }

  val map = forAll {
    (ass: List[List[Int]]) =>
      ListT.fromList(ass).map(_ * 2).toList must_===(ass.map(_.map(_ * 2)))
  }

  val flatMap = forAll {
    (ass: List[List[Int]]) =>
      (ListT.fromList(ass).flatMap(number => ListT.fromList(List(List(number.toFloat)))).toList
      must_===(ass.map(_.flatMap(number => List(number.toFloat)))))
  }

  // Exists to ensure that fromList and map don't stack overflow.
  val `large map` = forAll {
    val list = (0 to 400).toList.map(_ => (0 to 400).toList)
    ListT.fromList(list).map(_ * 2).toList must_===(list.map(_.map(_ * 2)))
  }
  
  val listT = forAll {
    (ass: Option[List[Int]]) =>
      ListT.listT(ass).run == ass
  }

  val testLaws = Properties.list(
    laws.equal.all[ListTOpt[Int]],
    laws.monoid.all[ListTOpt[Int]],
    laws.monadPlus.all[ListTOpt]
  )

  object instances {
    def semigroup[F[_]: Monad, A] = Semigroup[ListT[F, A]]
    def monoid[F[_]: Monad, A] = Monoid[ListT[F, A]]
    def monad[F[_]: Monad] = Monad[ListT[F, ?]]
    def functor[F[_]: Functor] = Functor[ListT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_]: Monad] = Functor[ListT[F, ?]]
  }
}
