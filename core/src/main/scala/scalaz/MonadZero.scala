package scalaz

////
/**
 *
 */
////
trait MonadZero[F[_]] extends Monad[F] { self =>
  ////

  // derived functions

  ////
  val monadZeroSyntax = new scalaz.syntax.MonadZeroSyntax[F] { def F = MonadZero.this }
}

object MonadZero {
  @inline def apply[F[_]](implicit F: MonadZero[F]): MonadZero[F] = F

  ////

  ////
}
