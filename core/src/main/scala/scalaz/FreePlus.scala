package scalaz

object FreePlus extends FreePlusInstances {
  private[scalaz] final case class Pure[F[_], A](a: A) extends FreePlus[F, A]
  private[scalaz] final case class Suspend[F[_], A](a: F[FreePlus[F, A]]) extends FreePlus[F, A]
  private[scalaz] final case class Plus[F[_], A](a: IList[FreePlus[F, A]]) extends FreePlus[F, A]

  private[this] val nil = Plus[Nothing, Nothing](INil())

  def empty[F[_], A]: FreePlus[F, A] = nil.asInstanceOf[FreePlus[F, A]]
}

sealed abstract class FreePlus[F[_], A] {
  import FreePlus._

  def map[B](f: A => B)(implicit F: Functor[F]): FreePlus[F, B] = this match {
    case Pure(a)    => Pure(f(a))
    case Suspend(a) => Suspend(F.map(a)(_.map(f)))
    case Plus(a)    => Plus(a.map(_.map(f)))
  }

  def flatMap[B](f: A => FreePlus[F, B])(implicit F: Functor[F]): FreePlus[F, B] = this match {
    case Pure(a)    => f(a)
    case Suspend(a) => Suspend(F.map(a)(_.flatMap(f)))
    case Plus(a)    => Plus(a.map(_.flatMap(f)))
  }

  def plus(that: FreePlus[F, A]): FreePlus[F, A] = (this, that) match {
    case (Plus(INil()), _           ) => that
    case (_           , Plus(INil())) => this 
    case (Plus(a)     , Plus(b)     ) => Plus(a ::: b)
    case (_           , _           ) => Plus(this :: that :: IList.empty)
  }
  
}

sealed abstract class FreePlusInstances {
  import FreePlus._

  implicit def freePlusMonadPlus[F[_]: Functor]: MonadPlus[({type λ[α] = FreePlus[F, α]})#λ] =
    new FreePlusMonadPlus[F] {
      def F = implicitly
    }

  implicit def freePlusEqual[F[_], A](implicit A: Equal[A], N: Equal ~> ({type λ[α] = Equal[F[α]]})#λ, F: Functor[F]): Equal[FreePlus[F, A]] = {
    implicit val f: Equal[FreePlus[F, A]] = freePlusEqual[F, A]
    Equal.equal{
      case (Pure(a)   , Pure(b)   ) => A.equal(a, b)
      case (Suspend(a), Suspend(b)) => N(freePlusEqual[F, A]).equal(a, b)
      case (Plus(a)   , Plus(b)   ) => IList.equal(freePlusEqual[F, A]).equal(a, b)
      case (_         , _         ) => false
    }
  }
}

private trait FreePlusMonadPlus[F[_]] extends MonadPlus[({type λ[α] = FreePlus[F, α]})#λ] {
  import FreePlus._

  implicit def F: Functor[F]

  override final def map[A, B](fa: FreePlus[F, A])(f: A => B) = fa map f

  def point[A](a: => A) = Pure(a)

  def bind[A, B](fa: FreePlus[F, A])(f: A => FreePlus[F, B]) = fa flatMap f

  def plus[A](a: FreePlus[F, A], b: => FreePlus[F, A]) = a plus b

  def empty[A] = FreePlus.empty[F, A]
}

