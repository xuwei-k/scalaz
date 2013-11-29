package scalaz

////
/**
 *
 */
////
trait BiinvariantFunctor[F[_, _]]  { self =>
  ////

  def bixmap[A, B, C, D](fab: F[A, B], ac: A => C, ca: C => A, bd: B => D, db: D => B): F[C, D]

  // derived functions

  ////
  val biinvariantFunctorSyntax = new scalaz.syntax.BiinvariantFunctorSyntax[F] { def F = BiinvariantFunctor.this }
}

object BiinvariantFunctor {
  @inline def apply[F[_, _]](implicit F: BiinvariantFunctor[F]): BiinvariantFunctor[F] = F

  ////

  ////
}
