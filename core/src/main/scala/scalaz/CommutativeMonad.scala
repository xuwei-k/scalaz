package scalaz

////
/**
 * [[http://www.haskell.org/haskellwiki/Monad#Commutative_monads]]
 */
////
trait CommutativeMonad[F[_]] extends Monad[F] { self =>
  ////

  // derived functions

  trait CommutativeMonadLaw extends MonadLaw {
    def commutative[A, B](fa: F[A], fb: F[B])(implicit E: Equal[F[(A, B)]]): Boolean =
      E.equal(tuple2(fa, fb), map(tuple2(fb, fa))(_.swap))
  }

  def commutativeMonadLaw = new CommutativeMonadLaw {}
  ////
  val commutativeMonadSyntax = new scalaz.syntax.CommutativeMonadSyntax[F] { def F = CommutativeMonad.this }
}

object CommutativeMonad {
  @inline def apply[F[_]](implicit F: CommutativeMonad[F]): CommutativeMonad[F] = F

  ////

  ////
}
