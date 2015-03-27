package scalaz

/** IListT monad transformer.
 * @see [[https://wiki.haskell.org/ListT_done_right_alternative]]
 */
final case class IListT[F[_], A](run: F[Maybe[(A, IListT[F, A])]]){

  private[this] def reverse(implicit F: Monad[F]): F[IList[A]] = {
    def loop(xs: IListT[F, A], ys: IList[A]): F[IList[A]] =
      F.bind(xs.run) {
        case Maybe.Empty() =>
          F.point(ys)
        case Maybe.Just((h, t)) =>
          loop(t, h :: ys)
      }
    loop(this, IList.empty)
  }

  def toIList(implicit F: Monad[F]): F[IList[A]] =
    F.map(this.reverse)(_.reverse)

  def map[B](f: A => B)(implicit F: Functor[F]): IListT[F, B] =
    IListT(F.map(run)(_.map{ case (a, b) => f(a) -> b.map(f) }))

  def flatMap[B](f: A => IListT[F, B])(implicit F: Monad[F]): IListT[F, B] =
    IListT(
      fold(F.point(Maybe.empty[(B, IListT[F, B])])){ (x, l) =>
        (f(x) ++ IListT(l)).run
      }
    )

  def ++(that: IListT[F, A])(implicit F: Monad[F]): IListT[F, A] =
    IListT(F.bind(run){
      case Maybe.Empty() =>
        that.run
      case Maybe.Just((h, t)) =>
        import std.function._, syntax.arrow._
        F.point(Maybe.just((that ++ _).second[A].apply(h, t)))
    })

  def fold[B](n: F[B])(c: (A, F[B]) => F[B])(implicit F: Bind[F]): F[B] =
    F.bind(run){
      case Maybe.Empty() =>
        n
      case Maybe.Just((x, l)) =>
        c(x, l.fold(n)(c))
    }
}


sealed abstract class IListTInstances2 {
  implicit final def IListTFunctor[F[_]](implicit F0: Functor[F]): Functor[IListT[F, ?]] =
    new IListTFunctor[F]{
      implicit def F = F0
    }
}

sealed abstract class IListTInstances1 extends IListTInstances2 {
  implicit final def IListTMonoid[F[_], A](implicit F0: Monad[F]): Monoid[IListT[F, A]] =
    IListT.IListTMonadPlus[F].monoid[A]
}

sealed abstract class IListTInstances extends IListTInstances1 {

  implicit final def IListTMonadPlus[F[_]](implicit F0: Monad[F]): MonadPlus[IListT[F, ?]] =
    new IListTMonadPlus[F] {
      implicit def F = F0
    }

  implicit final val iListTHoist: Hoist[IListT] =
    IListTHoist

}

object IListT extends IListTInstances {

  def empty[F[_], A](implicit F: Applicative[F]): IListT[F, A] =
    IListT(F.point(Maybe.empty))

  def liftIList[F[_]](implicit F: Applicative[F]): IList ~> IListT[F, ?] =
    new (IList ~> IListT[F, ?]) {
      def apply[A](list: IList[A]) =
        list match {
          case INil() =>
            IListT(F.point(Maybe.empty))
          case ICons(x, xs) =>
            IListT(F.point(Maybe.just((x, apply(xs)))))
        }
    }
}

private trait IListTFunctor[F[_]] extends Functor[IListT[F, ?]] {
  implicit def F: Functor[F]

  override final def map[A, B](fa: IListT[F, A])(f: A => B) = fa map f
}

private trait IListTMonadPlus[F[_]] extends MonadPlus[IListT[F, ?]] with IListTFunctor[F] {
  implicit def F: Monad[F]

  override def bind[A, B](fa: IListT[F, A])(f: A => IListT[F, B]) =
    fa flatMap f

  override def point[A](a: => A) =
    IListT(F.point(Maybe.just((a, IListT.empty[F, A]))))

  def empty[A] =
    IListT.empty[F, A]

  def plus[A](a: IListT[F, A], b: => IListT[F, A]) =
    a ++ b
}

private trait IListTMonoid[F[_], A] extends Monoid[IListT[F, A]] {
  implicit def F: Monad[F]

  override def append(f1: IListT[F, A], f2: => IListT[F, A]) =
    f1 ++ f2

  override def zero =
    IListT.empty[F, A]
}

private object IListTHoist extends Hoist[IListT] {
  import IListT._
  
  override def apply[G[_]: Monad] =
    IListTMonadPlus[G]

  override def liftM[G[_], A](a: G[A])(implicit G: Monad[G]) =
    IListT(G.map(a)(x => Maybe.just((x, IListT.empty))))

  override def hoist[M[_], N[_]](f: M ~> N)(implicit M: Monad[M]) =
    new (IListT[M, ?] ~> IListT[N, ?]) {
      def apply[A](a: IListT[M, A]) =
        IListT(f(M.map(a.run)(_.map{
          case (h, t) => h -> apply(t)
        })))
    }
}
