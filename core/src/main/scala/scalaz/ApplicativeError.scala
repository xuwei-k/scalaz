package scalaz

////
/*
*
*/
////
trait ApplicativeError[F[_], S] extends Applicative[F] { self =>
  ////
  def raiseError[A](e:S):F[A]
  def handleError[A](fa: F[A])(f: S => F[A]): F[A]

  trait ApplicativeErrorLaws{
    def raisedErrorsHandled[A](e: S, f: S => F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
  }

  def applicativeErrorLaws: ApplicativeErrorLaws = new ApplicativeErrorLaws {}
  ////
  val applicativeErrorSyntax: scalaz.syntax.ApplicativeErrorSyntax[F, S] =
    new scalaz.syntax.ApplicativeErrorSyntax[F, S] { def F = ApplicativeError.this }
}

object ApplicativeError {
  @inline inline def apply[F[_], S](implicit F: ApplicativeError[F, S]): ApplicativeError[F, S] = F

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: ApplicativeError[G, E]): ApplicativeError[F, E] =
    new IsomorphismApplicativeError[F, G, E] {
      override def G: ApplicativeError[G, E] = A
      override def iso: F <~> G = D
    }

  ////
  ////
}

trait IsomorphismApplicativeError[F[_], G[_], S] extends ApplicativeError[F, S] with IsomorphismApplicative[F, G]{
  implicit def G: ApplicativeError[G, S]
  ////
  override def raiseError[A](e: S): F[A] =
    iso.from(G.raiseError(e))

  override def handleError[A](fa: F[A])(f: S => F[A]): F[A] =
    iso.from(G.handleError(iso.to(fa))(s => iso.to(f(s))))
  ////
}
