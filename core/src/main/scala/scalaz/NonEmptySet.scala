package scalaz

import scala.annotation.tailrec
import scalaz.ISet.{Tip, Bin}

final case class NonEmptySet[A](run: ISet.Bin[A]) {

  def member(x: A)(implicit o: Order[A]): Boolean =
    run.member(x)

  def notMember(x: A)(implicit o: Order[A]): Boolean =
    run.notMember(x)

  def lookupLT(x: A)(implicit o: Order[A]): Option[A] =
    run.lookupLT(x)

  def lookupGT(x: A)(implicit o: Order[A]): Option[A] =
    run.lookupGT(x)

  def lookupLE(x: A)(implicit o: Order[A]): Option[A] =
    run.lookupLE(x)

  def lookupGE(x: A)(implicit o: Order[A]): Option[A] =
    run.lookupGE(x)

  def insert(x: A)(implicit o: Order[A]): NonEmptySet[A] =
    NonEmptySet(run.insert(x).asInstanceOf[ISet.Bin[A]])

  def delete(x: A)(implicit o: Order[A]): ISet[A] =
    run.delete(x)

  def filter(p: A => Boolean): ISet[A] =
    run.filter(p)

  def map[B: Order](f: A => B): NonEmptySet[B] =
    NonEmptySet(run.map(f).asInstanceOf[ISet.Bin[B]])

  def foldRight[B](z: B)(f: (A, B) => B): B =
    run.foldRight(z)(f)

  def foldLeft[B](z: B)(f: (B, A) => B): B =
    run.foldLeft(z)(f)

  def findMin: A = {
    @tailrec
    def loop(bin: ISet.Bin[A]): A = bin match {
      case Bin(x, Tip(), _) => x
      case Bin(_, x @ Bin(_, _, _), _) => loop(x)
    }
    loop(run)
  }

  def findMax: A = {
    @tailrec
    def loop(bin: ISet.Bin[A]): A = bin match {
      case Bin(x, _, Tip()) => x
      case Bin(_, _, x @ Bin(_, _, _)) => loop(x)
    }
    loop(run)
  }

  def deleteFindMin: (A, ISet[A]) =
    run.deleteFindMin

  def deleteFindMax: (A, ISet[A]) =
    run.deleteFindMax

  def toList: List[A] =
    run.toList

  def toIList: IList[A] =
    foldRight(IList.empty[A])(_ :: _)

  def toNel: NonEmptyList[A] =
    (toIList: @unchecked) match {
      case ICons(h, t) => NonEmptyList.nel(h, t)
    }
}

object NonEmptySet {
  def singleton[A](a: A): NonEmptySet[A] =
    NonEmptySet(ISet.Bin(a, ISet.empty, ISet.empty))
}
