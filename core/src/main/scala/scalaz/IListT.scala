package scalaz

/** IListT monad transformer.
 */
final case class IListT[M[_], A](run: M[IList[A]]){

  def uncons(implicit M: Applicative[M]): M[Maybe[(A, IListT[M, A])]] =
    M.map(run){
      case INil() => Maybe.empty
      case ICons(h, t) => Maybe.just((h, IListT(M.point(t))))
    }

  def ::(a: A)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(list => a :: list))

  def isEmpty(implicit M: Functor[M]): M[Boolean] =
    M.map(run)(_.isEmpty)

  def headOption(implicit M: Functor[M]): OptionT[M, A] =
    OptionT(M.map(run)(_.headOption))

  def headMaybe(implicit M: Functor[M]): MaybeT[M, A] =
    MaybeT(M.map(run)(_.headMaybe))
  
  def filter(p: A => Boolean)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(_.filter(p)))
  
  def drop(n: Int)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(_.dropWhile(p)))
  
  def take(n: Int)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]): IListT[M, A] =
    IListT(M.map(run)(_.takeWhile(p)))

  def ++(bs: => IListT[M, A])(implicit M: Apply[M]): IListT[M, A] =
    IListT(M.apply2(run, bs.run)(_ ++ _))

  def flatMap[B](f: A => IListT[M, B])(implicit M: Monad[M]): IListT[M, B] =
    IListT(M.bind(run){
      case INil() => M.point(IList.empty)
      case ICons(h, t) => t.foldLeft(f(h))((b, a) => b ++ f(a)).run
    })

  def flatMapF[B](f: A => M[IList[B]])(implicit M: Monad[M]): IListT[M, B] =
    flatMap(a => IListT(f(a)))

  def map[B](f: A => B)(implicit M: Functor[M]): IListT[M, B] =
    IListT(M.map(run)(_.map(f)))

  def foldLeft[B](z: B)(f: (B, A) => B)(implicit M: Functor[M]): M[B] =
    M.map(run)(_.foldLeft(z)(f))

  def toIList: M[IList[A]] =
    run

  def toList(implicit M: Functor[M]): M[List[A]] =
    M.map(run)(_.toList)

  def foldRight[B](z: B)(f: (A, B) => B)(implicit M: Functor[M]): M[B] =
    M.map(run)(_.foldRight(z)(f))

  def length(implicit M: Functor[M]): M[Int] =
    M.map(run)(_.length)
}

sealed abstract class IListTInstances2 {
  implicit def IListTFunctor[F[_]](implicit F0: Functor[F]): Functor[IListT[F, ?]] =
    new IListTFunctor[F]{
      implicit def F = F0
    }

  implicit def IListTSemigroup[F[_], A](implicit F0: Apply[F]): Semigroup[IListT[F, A]] =
    new IListTSemigroup[F, A]{
      implicit def F = F0
    }
}

sealed abstract class IListTInstances1 extends IListTInstances2 {
  implicit def IListTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[IListT[F, A]] =
    new IListTMonoid[F, A] {
      implicit def F = F0
    }
}

sealed abstract class IListTInstances extends IListTInstances1 {
  implicit def IListTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[IListT[F, ?]] =
    new IListTMonadPlus[F] {
      implicit def F = F0
    }

  implicit def IListTEqual[F[_], A](implicit E: Equal[F[IList[A]]]): Equal[IListT[F, A]] =
    E.contramap((_: IListT[F, A]).toIList)

  implicit def IListTShow[F[_], A](implicit E: Show[F[IList[A]]]): Show[IListT[F, A]] =
    Contravariant[Show].contramap(E)((_: IListT[F, A]).toIList)

  implicit val iListTHoist: Hoist[IListT] =
    IListTHoist
}

object IListT extends IListTInstances {
  def iListT[M[_]]: (λ[α => M[IList[α]]] ~> IListT[M, ?]) =
    new (λ[α => M[IList[α]]] ~> IListT[M, ?]) {
      def apply[A](a: M[IList[A]]) = IListT(a)
    }

  def empty[M[_], A](implicit M: Applicative[M]): IListT[M, A] =
    IListT(M.point(IList.empty))
}

//
// Implementation traits for type class instances
//

private trait IListTFunctor[F[_]] extends Functor[IListT[F, ?]] {
 implicit def F: Functor[F]
 override def map[A, B](fa: IListT[F, A])(f: A => B): IListT[F, B] = fa map f
}

private trait IListTSemigroup[F[_], A] extends Semigroup[IListT[F, A]] {
 implicit def F: Apply[F]
 def append(f1: IListT[F, A], f2: => IListT[F, A]): IListT[F, A] = f1 ++ f2
}

private trait IListTMonoid[F[_], A] extends Monoid[IListT[F, A]] with IListTSemigroup[F, A] {
  implicit def F: Monad[F]

  def zero: IListT[F, A] = IListT.empty[F, A]
}

private trait IListTMonadPlus[F[_]] extends MonadPlus[IListT[F, ?]] with IListTFunctor[F] {
  implicit def F: Monad[F]

  def bind[A, B](fa: IListT[F, A])(f: A => IListT[F, B]): IListT[F, B] = fa flatMap f

  def point[A](a: => A): IListT[F, A] = a :: IListT.empty[F, A]

  def empty[A]: IListT[F, A] = IListT.empty[F, A]

  def plus[A](a: IListT[F, A], b: => IListT[F, A]): IListT[F, A] = a ++ b
}

private object IListTHoist extends Hoist[IListT] {
  import IListT._
  
  implicit override def apply[G[_]: Monad]: Monad[IListT[G, ?]] =
    IListTMonadPlus[G]
  
  override def liftM[G[_], A](a: G[A])(implicit G: Monad[G]) =
    IListT(G.map(a)(IList.single))
  
  override def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) =
    new (IListT[M, ?] ~> IListT[N, ?]) {
      def apply[A](a: IListT[M, A]) = IListT(f(a.run))
    }
}
