package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 *
 */
////
trait Derives[F[_]] extends CoapplicativeCodivide[F] with ApplicativeDivisible[F] { self =>
  ////

////
  val derivesSyntax = new scalaz.syntax.DerivesSyntax[F] { def F = Derives.this }
}

object Derives {
  @inline def apply[F[_]](implicit F: Derives[F]): Derives[F] = F

  ////

////
}

trait IsomorphismDerives[F[_], G[_]] extends Derives[F] with IsomorphismCoapplicativeCodivide[F, G] with IsomorphismApplicativeDivisible[F, G]{
  implicit def G: Derives[G]
////

////
}
