package scalaz

/**
 * ListT monad transformer.
 */

final case class ListT[M[_], A](run: M[List[A]]){
  def uncons(implicit M: Applicative[M]): M[Option[(A, ListT[M, A])]] = {
    M.map(run){list =>
      list match {
        case Nil => None
        case listHead :: listTail => Some(listHead, new ListT(M.point(listTail)))
      }
    }
  }

  def ::(a: A)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(list => a :: list))

  def isEmpty(implicit M: Functor[M]) : M[Boolean] = M.map(run)(_.isEmpty)

  def headOption(implicit M: Functor[M]) : OptionT[M, A] = new OptionT(M.map(run)(_.headOption))

  def headMaybe(implicit M: Functor[M]) : MaybeT[M, A] = new MaybeT(M.map(run)(l => Maybe.fromOption(l.headOption)))
  
  def tailM(implicit M: Applicative[M]) : M[ListT[M, A]] = M.map(uncons)(_.get._2)

  def filter(p: A => Boolean)(implicit M: Functor[M]): ListT[M, A] = new ListT(M.map(run)(_.filter(p)))
  
  def drop(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.drop(n)))

  def dropWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.dropWhile(p)))
  
  def take(n: Int)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.take(n)))

  def takeWhile(p: A => Boolean)(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.takeWhile(p)))

  def ++(bs: => ListT[M, A])(implicit M: Commutative[M]) : ListT[M, A] = new ListT(M.bind(run){list1 =>
    M.map(bs.run){list2 =>
      list1 ++ list2
    }
  })

  def flatMap[B](f: A => ListT[M, B])(implicit M: Commutative[M]) : ListT[M, B] = new ListT(M.bind(run) {
    case Nil => M.point(Nil)
    case nonEmpty => nonEmpty.map(f).reduce(_ ++ _).run
  })

  def flatMapF[B](f: A => M[List[B]])(implicit M: Commutative[M]) : ListT[M, B] = flatMap(f andThen ListT.apply)

  def map[B](f: A => B)(implicit M: Functor[M]) : ListT[M, B] = new ListT(M.map(run)(_.map(f)))

  /**Don't use iteratively! */
  def tail(implicit M: Functor[M]) : ListT[M, A] = new ListT(M.map(run)(_.tail))

  def foldLeft[B](z: => B)(f: (=> B, => A) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldLeft(z){(left, right) => f(left, right)})

  def toList : M[List[A]] = run

  def foldRight[B](z: => B)(f: (=> A, => B) => B)(implicit M: Functor[M]) : M[B] = M.map(run)(_.foldRight(z){(right, left) => f(right, left)})

  def length(implicit M: Functor[M]) : M[Int] = M.map(run)(_.length)
}

//
// Prioritized Implicits for type class instances
//

sealed abstract class ListTInstances2 {
  implicit def listTFunctor[F[_]](implicit F0: Functor[F]): Functor[ListT[F, ?]] =
    new ListTFunctor[F]{
      implicit def F: Functor[F] = F0
    }
}

sealed abstract class ListTInstances1 extends ListTInstances2 {
  implicit def listTMonoid[F[_], A](implicit F0: Commutative[F]): Monoid[ListT[F, A]] =
    new ListTMonoid[F, A] {
      override def F = F0
    }
}

sealed abstract class ListTInstances extends ListTInstances1 {
  implicit def listTMonadPlus[F[_]](implicit F0: Commutative[F]): MonadPlus[ListT[F, ?]] =
    new ListTMonadPlus[F] {
      implicit def F = F0
    }

  implicit def listTEqual[F[_], A](implicit E: Equal[F[List[A]]]): Equal[ListT[F, A]] =
    E.contramap((_: ListT[F, A]).toList)

  implicit def listTShow[F[_], A](implicit E: Show[F[List[A]]]): Show[ListT[F, A]] =
    Contravariant[Show].contramap(E)((_: ListT[F, A]).toList)
}

trait ListTFunctions {
  def listT[M[_]]: (λ[α => M[List[α]]] ~> ListT[M, ?]) =
    new (λ[α => M[List[α]]] ~> ListT[M, ?]) {
      def apply[A](a: M[List[A]]) = new ListT[M, A](a)
    }
  
  def empty[M[_], A](implicit M: Applicative[M]): ListT[M, A] =
    new ListT[M, A](M.point(Nil))

  def fromList[M[_], A](mas: M[List[A]]): ListT[M, A] =
    new ListT(mas)
}

object ListT extends ListTInstances with ListTFunctions

//
// Implementation traits for type class instances
//

private trait ListTFunctor[F[_]] extends Functor[ListT[F, ?]] {
 implicit def F: Functor[F]
 override def map[A, B](fa: ListT[F, A])(f: A => B): ListT[F, B] = fa map f
}

private trait ListTMonoid[F[_], A] extends Monoid[ListT[F, A]] {
  implicit def F: Commutative[F]

  def append(f1: ListT[F, A], f2: => ListT[F, A]): ListT[F, A] = f1 ++ f2
  def zero: ListT[F, A] = ListT.empty[F, A]
}

private trait ListTMonadPlus[F[_]] extends MonadPlus[ListT[F, ?]] with ListTFunctor[F] {
  implicit def F: Commutative[F]

  def bind[A, B](fa: ListT[F, A])(f: A => ListT[F, B]): ListT[F, B] = fa flatMap f

  def point[A](a: => A): ListT[F, A] = a :: ListT.empty[F, A]

  def empty[A]: ListT[F, A] = ListT.empty[F, A]

  def plus[A](a: ListT[F, A], b: => ListT[F, A]): ListT[F, A] = a ++ b
}
