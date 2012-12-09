package scalaz

////
/**
 *
 */
////
trait Monoidal[F[_]] extends Functor[F] { self =>
  ////

  // derived functions

  ////
  val monoidalSyntax = new scalaz.syntax.MonoidalSyntax[F] { def F = Monoidal.this }
}

object Monoidal {
  @inline def apply[F[_]](implicit F: Monoidal[F]): Monoidal[F] = F

  ////

  ////
}

