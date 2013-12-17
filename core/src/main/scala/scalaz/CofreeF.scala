package scalaz

/** This is the base functor of the cofree comonad transformer.
 * [[https://github.com/ekmett/free/blob/b229616f69/src/Control/Comonad/Trans/Cofree.hs#L50-L52]]
 */
final case class CofreeF[F[_], A, B](head: A, tail: F[B]){
  def map[C](f: B => C)(implicit F: Functor[F]): CofreeF[F, A, C] =
    copy(tail = F.map(tail)(f))

  def bimap[C, D](f: A => C)(g: B => D)(implicit F: Functor[F]): CofreeF[F, C, D] =
    CofreeF(f(head), F.map(tail)(g))

  def bifoldMap[C](f: A => C)(g: B => C)(implicit F: Foldable[F], C: Monoid[C]): C =
    C.append(f(head), F.foldMap(tail)(g))

  def bifoldMap1[C](f: A => C)(g: B => C)(implicit F: Foldable[F], C: Semigroup[C]): C = {
    val h = f(head)
    F.foldMap1Opt(tail)(g).map(C.append(h, _)).getOrElse(h)
  }

  def bitraverse[C, D, G[_]](f: A => G[C])(g: B => G[D])(implicit G: Applicative[G], F: Traverse[F]): G[CofreeF[F, C, D]] =
    G.apply2(f(head), F.traverse(tail)(g))(CofreeF.apply)

  private[scalaz] def trans[G[_]](f: F ~> G): CofreeF[G, A, B] =
    copy(tail = f(tail))
}

object CofreeF extends CofreeFInstances

sealed abstract class CofreeFInstances extends CofreeFInstances0{
  implicit def cofreeFTraverse[F[_]: Traverse, A]: Traverse[({type λ[α] = CofreeF[F, A, α]})#λ] =
    new CofreeFTraverse[F, A]{
      def F = implicitly
    }

  implicit def cofreeFBitraverse[F[_]: Traverse, A]: Bitraverse[({type λ[α, β] = CofreeF[F, α, β]})#λ] =
    new CofreeFBitraverse[F]{
      def F = implicitly
    }

  implicit def cofreeFOrder[F[_], A, B](implicit A0: Order[A], F0: Order[F[B]]): Order[CofreeF[F, A, B]] =
    new CofreeFOrder[F, A, B]{
      def A = A0
      def F = F0
    }
}

sealed abstract class CofreeFInstances0{
  implicit def cofreeFEqual[F[_], A, B](implicit A0: Equal[A], F0: Equal[F[B]]): Equal[CofreeF[F, A, B]] =
    new CofreeFEqual[F, A, B]{
      def A = A0
      def F = F0
    }

  implicit def cofreeFFunctor[F[_]: Functor, A]: Functor[({type λ[α] = CofreeF[F, A, α]})#λ] =
    new CofreeFFunctor[F, A]{
      def F = implicitly
    }

  implicit def cofreeFFoldable[F[_]: Foldable, A]: Foldable[({type λ[α] = CofreeF[F, A, α]})#λ] =
    new CofreeFFoldable[F, A]{
      def F = implicitly
    }

  implicit def cofreeFBifunctor[F[_]: Functor, A]: Bifunctor[({type λ[α, β] = CofreeF[F, α, β]})#λ] =
    new CofreeFBifunctor[F]{
      def F = implicitly
    }

  implicit def cofreeFBifoldable[F[_]: Foldable, A]: Bifoldable[({type λ[α, β] = CofreeF[F, α, β]})#λ] =
    new CofreeFBifoldable[F]{
      def F = implicitly
    }
}

private trait CofreeFFunctor[F[_], X] extends Functor[({type λ[α] = CofreeF[F, X, α]})#λ]{
  implicit def F: Functor[F]

  override final def map[A, B](fa: CofreeF[F, X, A])(f: A => B) = fa.map(f)
}

private trait CofreeFFoldable[F[_], X] extends Foldable.FromFoldMap[({type λ[α] = CofreeF[F, X, α]})#λ]{
  def F: Foldable[F]

  override final def foldMap[A, B: Monoid](fa: CofreeF[F, X, A])(f: A => B) = F.foldMap(fa.tail)(f)
}

private trait CofreeFTraverse[F[_], X] extends Traverse[({type λ[α] = CofreeF[F, X, α]})#λ] with CofreeFFunctor[F, X] with CofreeFFoldable[F, X]{
  def F: Traverse[F]

  def traverseImpl[G[_], A, B](fa: CofreeF[F, X, A])(f: A => G[B])(implicit G: Applicative[G]) =
    G.map(F.traverse(fa.tail)(f))(CofreeF(fa.head, _))
}

private trait CofreeFBifunctor[F[_]] extends Bifunctor[({type λ[α, β] = CofreeF[F, α, β]})#λ]{
  implicit def F: Functor[F]

  final override def bimap[A, B, C, D](fab: CofreeF[F, A, B])(f: A => C, g: B => D) = fab.bimap(f)(g)
}

private trait CofreeFBifoldable[F[_]] extends Bifoldable[({type λ[α, β] = CofreeF[F, α, β]})#λ]{
  implicit def F: Foldable[F]

  final override def bifoldMap[A, B, M: Monoid](fab: CofreeF[F, A, B])(f: A => M)(g: B => M) = fab.bifoldMap(f)(g)

  final override def bifoldRight[A, B, C](fab: CofreeF[F, A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C) =
    f(fab.head, F.foldRight(fab.tail, z)(g))
}

private trait CofreeFBitraverse[F[_]] extends Bitraverse[({type λ[α, β] = CofreeF[F, α, β]})#λ] with CofreeFBifoldable[F] with CofreeFBifunctor[F]{
  implicit def F: Traverse[F]

  def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: CofreeF[F, A, B])(f: A => G[C], g: B => G[D]) = fab.bitraverse(f)(g)
}

private trait CofreeFEqual[F[_], A, B] extends Equal[CofreeF[F, A, B]]{
  def A: Equal[A]
  def F: Equal[F[B]]

  override final def equal(a: CofreeF[F, A, B], b: CofreeF[F, A, B]) = A.equal(a.head, b.head) && F.equal(a.tail, b.tail)
}

private abstract class CofreeFOrder[F[_], A, B] extends Order[CofreeF[F, A, B]] with CofreeFEqual[F, A, B]{
  def A: Order[A]
  def F: Order[F[B]]

  override def order(a: CofreeF[F, A, B], b: CofreeF[F, A, B]) =
    Semigroup[Ordering].append(A.order(a.head, b.head), F.order(a.tail, b.tail))
}
