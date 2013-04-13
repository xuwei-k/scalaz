package scalaz

////
/**
 *
 */
////
trait ComonadApply[F[_]] extends Comonad[F] { self =>
  ////

  // derived functions

  ////
  val comonadApplySyntax = new scalaz.syntax.ComonadApplySyntax[F] { def F = ComonadApply.this }
}

object ComonadApply {
  @inline def apply[F[_]](implicit F: ComonadApply[F]): ComonadApply[F] = F

  ////

  ////
}
