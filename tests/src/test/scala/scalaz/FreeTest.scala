package scalaz

import Isomorphism._
import std.AllInstances._
import Free._

object FreeTest extends SpecLite {

  type PairOpt[α] = Option[(α, α)]
  type FList[A] = Free[PairOpt, A]

  implicit val pairOptInstance: Functor[PairOpt] with Foldable[PairOpt] =
    new Functor[PairOpt] with Foldable.FromFoldr[PairOpt]{
      def map[A, B](fa: PairOpt[A])(f: A => B) =
        fa.map(t => (f(t._1), f(t._2)))

      def foldRight[A, B](fa: PairOpt[A], z: => B)(f: (A, => B) => B) =
        fa.map(t => f(t._1, f(t._2, z))).getOrElse(z)
    }

  object FList{
    private[this] val _nil = Suspend[PairOpt, Nothing](None)

    def nil[A]: FList[A] = _nil.asInstanceOf[FList[A]]

    def cons[A](head: A, tail: FList[A]): FList[A] =
      Suspend[PairOpt, A](Option(Return[PairOpt, A](head) -> tail))

    def apply[A](xs: A*): FList[A] = listFListIso.from(xs.toList)
  }

  val listFListIso: FList <~> List =
    new IsoFunctorTemplate[FList, List]{
      def to[A](ga: FList[A]) = {
        Foldable[FList].foldRight(ga, List[A]())(_ :: _)
      }

      def from[A](fa: List[A]) =
        fa.foldRight(FList.nil[A])(FList.cons[A])
    }

  implicit def FListInstance0[A: Equal]: Equal[FList[A]] =
    new IsomorphismEqual[FList[A], List[A]]{
      def G = Equal[List[A]]
      def iso = listFListIso.unlift[A]
    }

}


