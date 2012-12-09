package scalaz

////
/**
 *
 */
////
trait Monoidal[F[_]] extends Functor[F] { self =>
  ////

  // derived functions
  def unit: F[Unit]

  def **[A, B](fa: F[A], fb: F[B]): F[(A, B)]

  override def map[A, B](fa: F[A])(f: A => B): F[B] = applicative.map(fa)(f)

  def applicative: Applicative[F] = new Applicative[F]{

    def point[A](a: => A): F[A] = map(unit)(_ => a)

    def ap[A, B](fa: => F[A])(f: => F[A => B]): F[B] = map(**(fa, f)){case (a,b) => b(a)} 
  }

  trait MonoidalLaw extends FunctorLaw{
    def naturality[A, B](a: F[A], b: F[B], f: B => A, g: A => B)(implicit FAB: Equal[F[(A,B)]]): Boolean =
      FAB.equal(**(map(b)(f), map(a)(g)), map(**(a, b)){case (x, y) => (f(y), g(x))})
/*
    def leftIdentity[A](fa: F[A])(implicit EA: Equal[F[A]]): Boolean = EA.equal(**(unit, fa), fa)

    def rightIdentity[A](fa: F[A])(implicit EA: Equal[F[A]]): Boolean = EA.equal(**(fa, unit), fa)

    def associativity[A](a: F[A], b: F[A], c: F[A])(implicit EA: Equal[F[A]]): Boolean =
      EA.equal(**(a, **(b, c)), **(**(a, b), c))
*/
  }

  def monoidalLaw = new MonoidalLaw {}

  ////
  val monoidalSyntax = new scalaz.syntax.MonoidalSyntax[F] { def F = Monoidal.this }
}

object Monoidal {
  @inline def apply[F[_]](implicit F: Monoidal[F]): Monoidal[F] = F

  ////

  ////
}

