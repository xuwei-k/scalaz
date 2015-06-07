package scalaz

////
/**
 *
 */
////
trait Commutative[F[_]] extends Monad[F] { self =>
  ////

  trait CommutativeLaw extends MonadLaw {
    def commutative[A, B](fa: F[A], fb: F[B])(implicit E: Equal[F[(A, B)]]): Boolean = {
      val x = bind(fa){ a =>
        bind(fb){ b => point((a, b)) }
      }
      val y = bind(fb){ b =>
        bind(fa){ a => point((a, b)) }
      }
      E.equal(x, y)
    }
  }
  def commutativeLow = new CommutativeLaw {}

  ////
  val commutativeSyntax = new scalaz.syntax.CommutativeSyntax[F] { def F = Commutative.this }
}

object Commutative {
  @inline def apply[F[_]](implicit F: Commutative[F]): Commutative[F] = F

  ////

  ////
}
