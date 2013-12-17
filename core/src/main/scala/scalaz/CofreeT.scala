package scalaz

final case class CofreeT[F[_], W[_], A](run: W[CofreeF[F, A, CofreeT[F, W, A]]])

object CofreeT extends CofreeTInstances{

  final def coiterT[F[_], W[_], A](wa: W[A])(f: W[A] => F[W[A]])(implicit F: Functor[F], W: Comonad[W]): CofreeT[F, W, A] =
    CofreeT(W.cobind(wa)(w => CofreeF(W.copoint(w), F.map(f(w))(coiterT(_)(f)))))
}

sealed abstract class CofreeTInstances extends CofreeTInstances0{
  implicit def cofreeTComonad[F[_]: Functor, W[_]: Comonad]: Comonad[({type λ[α] = CofreeT[F, W, α]})#λ] =
    new CofreeTComonad[F, W] {
      def F = implicitly
      def W = implicitly
    }

  implicit def cofreeTComonadTrans[F[_]]: ComonadTrans[({type λ[α[_], β] = CofreeT[F, α, β]})#λ] =
    new CofreeTComonadTrans[F]{}

  implicit def cofreeTOrder[F[_], W[_], A](implicit F0: Order[W[CofreeF[F, A, CofreeT[F, W, A]]]]): Order[CofreeT[F, W, A]] =
    new CofreeTOrder[F, W, A]{
      def F = F0
    }
}

sealed abstract class CofreeTInstances0 extends CofreeTInstances1{
  implicit def cofreeTEqual[F[_], W[_], A](implicit F0: Equal[W[CofreeF[F, A, CofreeT[F, W, A]]]]): Equal[CofreeT[F, W, A]] =
    new CofreeTEqual[F, W, A]{
      def F = F0
    }

  implicit def cofreeTTraverse[F[_]: Traverse, W[_]: Traverse]: Traverse[({type λ[α] = CofreeT[F, W, α]})#λ] =
    new CofreeTTraverse[F, W] {
      def F = implicitly
      def W = implicitly
    }
}

sealed abstract class CofreeTInstances1{
  implicit def cofreeTFunctor[F[_]: Functor, W[_]: Functor]: Functor[({type λ[α] = CofreeT[F, W, α]})#λ] =
    new CofreeTFunctor[F, W] {
      def F = implicitly
      def W = implicitly
    }

  implicit def cofreeTFoldable[F[_]: Foldable, W[_]: Foldable]: Foldable[({type λ[α] = CofreeT[F, W, α]})#λ] =
    new CofreeTFoldable[F, W] {
      def F = implicitly
      def W = implicitly
    }
}

private trait CofreeTFunctor[F[_], W[_]] extends Functor[({type λ[α] = CofreeT[F, W, α]})#λ]{
  implicit def F: Functor[F]
  implicit def W: Functor[W]
  override final def map[A, B](fa: CofreeT[F, W, A])(f: A => B): CofreeT[F, W, B] =
    CofreeT(W.map(fa.run)(_.bimap(f)(map(_)(f))))
}

private trait CofreeTComonad[F[_], W[_]] extends Comonad[({type λ[α] = CofreeT[F, W, α]})#λ] with CofreeTFunctor[F, W]{
  implicit def W: Comonad[W]

  def copoint[A](fa: CofreeT[F, W, A]): A = W.copoint(fa.run).head

  def cobind[A, B](fa: CofreeT[F, W, A])(f: CofreeT[F, W, A] => B): CofreeT[F, W, B] =
    CofreeT(W.cobind(fa.run)(w =>
      CofreeF(f(CofreeT(w)), F.map(W.copoint(w).tail)(cobind(_)(f)))
    ))
}

private trait CofreeTFoldable[F[_], W[_]] extends Foldable.FromFoldMap[({type λ[α] = CofreeT[F, W, α]})#λ]{
  implicit def F: Foldable[F]
  def W: Foldable[W]

  override final def foldMap[A, B: Monoid](fa: CofreeT[F, W, A])(f: A => B): B =
    W.foldMap(fa.run)(_.bifoldMap(f)(foldMap(_)(f)))
}

private trait CofreeTTraverse[F[_], W[_]] extends Traverse[({type λ[α] = CofreeT[F, W, α]})#λ] with CofreeTFoldable[F, W] with CofreeTFunctor[F, W]{
  implicit def F: Traverse[F]
  def W: Traverse[W]

  override def traverseImpl[G[_], A, B](fa: CofreeT[F, W, A])(f: A => G[B])(implicit G: Applicative[G]): G[CofreeT[F, W, B]] =
    G.map(W.traverse(fa.run)(
      _.bitraverse(f)(traverse(_)(f))
    ))(CofreeT.apply)
}

private class CofreeTComonadTrans[F[_]] extends ComonadTrans[({type λ[α[_], β] = CofreeT[F, α, β]})#λ]{
  def lower[G[_], A](a: CofreeT[F, G, A])(implicit G: Cobind[G]): G[A] =
    G.map(a.run)(_.head)
}

private trait CofreeTEqual[F[_], W[_], A] extends Equal[CofreeT[F, W, A]]{
  def F: Equal[W[CofreeF[F, A, CofreeT[F, W, A]]]]

  override final def equal(a: CofreeT[F, W, A], b: CofreeT[F, W, A]) = F.equal(a.run, b.run)
}

private abstract class CofreeTOrder[F[_], W[_], A] extends Order[CofreeT[F, W, A]] with CofreeTEqual[F, W, A]{
  def F: Order[W[CofreeF[F, A, CofreeT[F, W, A]]]]

  def order(x: CofreeT[F, W, A], y: CofreeT[F, W, A]) = F.order(x.run, y.run)
}

