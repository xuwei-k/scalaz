package scalaz
package std

import collection.immutable.Seq

private[std] trait IterableSubtypeFoldable[I[X] <: Iterable[X]] extends Foldable[I] {

  override def foldMap[A,B](fa: I[A])(f: A => B)(implicit F: Monoid[B]) = foldLeft(fa, F.zero)((x,y) => Monoid[B].append(x, f(y)))

  override def foldRight[A, B](fa: I[A], b: => B)(f: (A, => B) => B) = fa.foldRight(b)(f(_, _))

  override def foldLeft[A, B](fa: I[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

  override def length[A](a: I[A]) = {
    var n = 0
    val i = a.iterator
    while (i.hasNext) {
      n = n + 1
      i.next
    }
    n
  }

  override final def toList[A](fa: I[A]) = fa.toList
  override final def toVector[A](fa: I[A]) = fa.toVector
  override final def toSet[A](fa: I[A]) = fa.toSet
  override final def toStream[A](fa: I[A]) = fa.toStream

  override final def empty[A](fa: I[A]) = fa.isEmpty

  override final def any[A](fa: I[A])(p: A => Boolean): Boolean =
    fa exists p

  override final def all[A](fa: I[A])(p: A => Boolean): Boolean =
    fa forall p
}
