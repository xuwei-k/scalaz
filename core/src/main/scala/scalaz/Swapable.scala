package scalaz

////
/**
 *
 */
////
trait Swapable[F[_]] extends Monad[F] { self =>
  ////

  // derived functions

  ////
  val swapableSyntax = new scalaz.syntax.SwapableSyntax[F] { def F = Swapable.this }
}

object Swapable {
  @inline def apply[F[_]](implicit F: Swapable[F]): Swapable[F] = F

  ////

  ////
}
