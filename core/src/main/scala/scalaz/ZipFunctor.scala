package scalaz

////
/**
 *
 */
////
trait ZipFunctor[F[_]] extends Functor[F] with Zip[F] { self =>
  ////

  // derived functions

  trait ZipFunctorLaw extends FunctorLaw {

    /** [[https://github.com/ghc/packages-base/blob/ghc-7.6.3-release/Control/Monad/Zip.hs#L29]] */
    def naturality[A, B, C, D](a: F[A], b: F[B], f: A => C, g: B => D)(implicit E: Equal[F[(C, D)]]): Boolean = {
      import syntax.arrow._
      import std.function._
      E.equal(self.map(zip(a, b))(f *** g), zip(self.map(a)(f), self.map(b)(g)))
    }
  }

  def zipFunctorLaw = new ZipFunctorLaw {}

  ////
  val zipFunctorSyntax = new scalaz.syntax.ZipFunctorSyntax[F] { def F = ZipFunctor.this }
}

object ZipFunctor {
  @inline def apply[F[_]](implicit F: ZipFunctor[F]): ZipFunctor[F] = F

  ////

  ////
}
