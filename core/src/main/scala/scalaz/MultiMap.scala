package scalaz

final case class MultiMap[A, B] private (run: A ==>> ISet[B]) {

  def size: Int = run.size

  def isEmpty: Boolean = run.isEmpty

  def insert(k: A, v: B)(implicit A: Order[A], B: Order[B]): MultiMap[A, B] =
    MultiMap(
      run.alter(k, {
        case Some(set) => Some(set.insert(v))
        case None      => Some(ISet.singleton(v))
      })
    )

  def map[C](f: B => C)(implicit B: Order[B], C: Order[C]): MultiMap[A, C] =
    MultiMap(run.map(_.map(f)))

  def delete(k: A)(implicit A: Order[A]): MultiMap[A, B] =
    MultiMap(run.delete(k))

  def deleteValue(k: A, v: B)(implicit A: Order[A], B: Order[B]): MultiMap[A, B] =
    MultiMap(
      run.update(k, { set =>
        val a = set delete v
        if(a.isEmpty) None
        else Some(a)
      })
    )

  def member(k: A)(implicit A: Order[A]): Boolean =
    run.member(k)

  def lookup(k: A)(implicit A: Order[A]): ISet[B] =
    run.lookup(k).getOrElse(ISet.empty[B])

  def keys: List[A] =
    run.keys

  def values: List[ISet[B]] =
    run.values

  def valuesNel: List[NonEmptyList[B]] =
    run.values.map{ set =>
      val (b, bs) = set.deleteFindMax
      bs.foldRight(NonEmptyList.nel(b, Nil))(_ <:: _)
    }

  override def toString =
    run.toList.view.map{ case (k, v) => k -> ISet.setShow(Show.showA).shows(v) }.mkString("MultiMap(", ", ", ")")

  def foldLeft[C](z: C)(f: (C, B) => C): C =
    run.foldlWithKey(z)((acc, _, set) => set.foldl(acc)(f))

  def foldRight[C](z: C)(f: (B, C) => C): C =
    run.foldrWithKey(z)((_, set, acc) => set.foldr(acc)(f))

  def foldMap[C](f: B => C)(implicit C: Monoid[C]): C =
    run.foldlWithKey(C.zero)((acc, _, set) =>
      C.append(acc, Foldable[ISet].foldMap(set)(f))
    )
}

object MultiMap {

  def fromList[A, B](a: List[(A, List[B])])(implicit A: Order[A], B: Order[B]): MultiMap[A, B] =
    MultiMap(==>>.fromList(
      a.collect{case (k, v) if v.nonEmpty => k -> ISet.fromList(v)}
    ))

  implicit def multiMapInstance[K]: Foldable[({type λ[α] = MultiMap[K, α]})#λ] =
    new Foldable[({type λ[α] = MultiMap[K, α]})#λ] {
      override def foldLeft[A, B](fa: MultiMap[K, A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)
      def foldMap[A, B](fa: MultiMap[K, A])(f: A => B)(implicit F: Monoid[B]) =
        fa foldMap f
      def foldRight[A, B](fa: MultiMap[K, A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)((a, b) => f(a, b))
    }

}

