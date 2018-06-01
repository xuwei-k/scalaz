package scalaz

////
/**
 *
 */
////
trait CovariantDerives[F[_]] extends Derives[F] with Coapplicative[F] with Applicative[F] { self =>
  ////

  ////
  val covariantDerivesSyntax = new scalaz.syntax.CovariantDerivesSyntax[F] { def F = CovariantDerives.this }
}

object CovariantDerives {
  @inline def apply[F[_]](implicit F: CovariantDerives[F]): CovariantDerives[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: CovariantDerives[G]): CovariantDerives[F] =
    new IsomorphismCovariantDerives[F, G] {
      override def G: CovariantDerives[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}
