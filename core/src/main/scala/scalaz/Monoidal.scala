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

  ////
  val monoidalSyntax = new scalaz.syntax.MonoidalSyntax[F] { def F = Monoidal.this }
}

object Monoidal {
  @inline def apply[F[_]](implicit F: Monoidal[F]): Monoidal[F] = F

  ////

  ////
}

