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

  ////

////
}

trait IsomorphismCovariantDerives[F[_], G[_]] extends CovariantDerives[F] with IsomorphismDerives[F, G] with IsomorphismCoapplicative[F, G] with IsomorphismApplicative[F, G]{
  implicit def G: CovariantDerives[G]
////

////
}
