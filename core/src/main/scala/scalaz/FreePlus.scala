package scalaz

object FreePlus extends FreePlusInstances {
  private[scalaz] final case class Pure[F[_], A](a: A) extends FreePlus[F, A]
  private[scalaz] final case class Suspend[F[_], A](a: F[FreePlus[F, A]]) extends FreePlus[F, A]
  private[scalaz] final case class Plus[F[_], A](a: IList[FreePlus[F, A]]) extends FreePlus[F, A]
  private[scalaz] final case class Gosub[F[_], A, B](a: () => FreePlus[F, A], f: A => FreePlus[F, B]) extends FreePlus[F, B]

  private[this] val nil = Plus[Nothing, Nothing](INil())

  def pure[F[_], A](a: A): FreePlus[F, A] = Pure(a)
  def point[F[_]] = new (Id.Id ~> ({type λ[α] = FreePlus[F, α]})#λ) {
    def apply[A](a: A) = Pure(a)
  }
  def suspend[F[_], A](a: F[FreePlus[F, A]]): FreePlus[F, A] = Suspend(a)
  def plus[F[_], A](a: IList[FreePlus[F, A]]): FreePlus[F, A] = Plus(a)

  def empty[F[_], A]: FreePlus[F, A] = nil.asInstanceOf[FreePlus[F, A]]
}

sealed abstract class FreePlus[F[_], A] {
  import FreePlus._

  final def map[B](f: A => B)(implicit F: Functor[F]): FreePlus[F, B] =
    flatMap(a => Pure(f(a)))

  final def flatMap[B](f: A => FreePlus[F, B])(implicit F: Functor[F]): FreePlus[F, B] = this match {
    case Gosub(a, g) => Gosub(a, (x: Any) => Gosub(() => g(x), f))
    case a           => Gosub(() => a, f)
  }

  final def plus(that: FreePlus[F, A])(implicit F: Functor[F]): FreePlus[F, A] = (this.resume, that.resume) match {
    case (Right3(a)     , Left3(b)  ) => Plus(a :+ Pure[F, A](b))
    case (Left3(a)      , Right3(b) ) => Plus(Pure[F, A](a) +: b)
    case (Right3(a)     , Right3(b) ) => Plus(a ::: b)
    case (_             , _         ) => Plus(this :: that :: IList.empty)
  }
  
  @annotation.tailrec
  final def resume(implicit F: Functor[F]): Either3[A, F[FreePlus[F, A]], IList[FreePlus[F, A]]] = this match {
    case Pure(a)    => Left3(a)
    case Suspend(a) => Middle3(a)
    case Plus(a)    => Right3(a)
    case a Gosub f  => a() match {
      case Pure(b)    => f(b).resume
      case Suspend(b) => Middle3(F.map(b)(((_: FreePlus[F, Any]) flatMap f)))
      case Plus(b)    => Right3(b.map(_.flatMap(f)))
      case b Gosub g  => b().flatMap((x: Any) => g(x) flatMap f).resume
    }
  }

}

sealed abstract class FreePlusInstances {
  import FreePlus._

  implicit def freePlusMonadPlus[F[_]: Functor]: MonadPlus[({type λ[α] = FreePlus[F, α]})#λ] =
    new FreePlusMonadPlus[F] {
      def F = implicitly
    }

  implicit def freePlusEqual[F[_], A](implicit A: Equal[A], N: Equal ~> ({type λ[α] = Equal[F[α]]})#λ, F: Functor[F]): Equal[FreePlus[F, A]] = {
    Equal.equal{ (aa, bb) =>
      (aa.resume, bb.resume) match {
        case (Left3(a)  , Left3(b)  ) => A.equal(a, b)
        case (Middle3(a), Middle3(b)) => N(freePlusEqual[F, A]).equal(a, b)
        case (Right3(a) , Right3(b) ) => IList.equal(freePlusEqual[F, A]).equal(a, b)
        case (_         , _         ) => false
      }
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

