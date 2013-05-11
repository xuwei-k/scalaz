package scalaz

////
/**
 * @see [[http://web.cecs.pdx.edu/~mpj/pubs/composing.html Composing Monads]]
 */
////
trait Swapable[F[_]] extends Monad[F] { self =>
  ////

  // derived functions

  def swap[A, GA](gfa: F[GA])(implicit G: Unapply[Monad, GA]): G.M[F[G.A]]

  ////
  val swapableSyntax = new scalaz.syntax.SwapableSyntax[F] { def F = Swapable.this }
}

object Swapable {
  @inline def apply[F[_]](implicit F: Swapable[F]): Swapable[F] = F

  ////

  ////
}


