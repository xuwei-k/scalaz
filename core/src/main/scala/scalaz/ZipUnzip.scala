package scalaz

////
/**
 *
 */
////
trait ZipUnzip[F[_]] extends ZipFunctor[F] with Unzip[F] { self =>
  ////

  // derived functions

  trait ZipUnzipLaw extends ZipFunctorLaw {
    /** [[https://github.com/ghc/packages-base/blob/ghc-7.6.3-release/Control/Monad/Zip.hs#L33]] */
    def informationPreservation[A, B](a: F[A], b: F[B])(implicit E0: Equal[F[Unit]], E1: Equal[(F[A], F[B])]): Boolean =
      if(E0.equal(self.map(a)(Function.const(())), self.map(b)(Function.const(()))))
        E1.equal(unzip(zip(a, b)), (a, b))
      else
        true
  }
  def zipUnzipLaw = new ZipUnzipLaw {}

  ////
  val zipUnzipSyntax = new scalaz.syntax.ZipUnzipSyntax[F] { def F = ZipUnzip.this }
}

object ZipUnzip {
  @inline def apply[F[_]](implicit F: ZipUnzip[F]): ZipUnzip[F] = F

  ////

  ////
}
