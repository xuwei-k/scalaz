package scalaz

////
/**
 *
 */
////
trait Commutative[F[_]] extends Monad[F] { self =>
  ////

  ////
  val commutativeSyntax = new scalaz.syntax.CommutativeSyntax[F] { def F = Commutative.this }
}

object Commutative {
  @inline def apply[F[_]](implicit F: Commutative[F]): Commutative[F] = F

  ////

  ////
}
