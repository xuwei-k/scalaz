package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ZipFunctor` */
trait ZipFunctorOps[F[_],A] extends Ops[F[A]] {
  implicit def F: ZipFunctor[F]
  ////

  ////
}

trait ToZipFunctorOps0 {
  implicit def ToZipFunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply[ZipFunctor, FA]) =
    new ZipFunctorOps[F0.M,F0.A] { def self = F0(v); implicit def F: ZipFunctor[F0.M] = F0.TC }

}

trait ToZipFunctorOps extends ToZipFunctorOps0 with ToFunctorOps with ToZipOps {
  implicit def ToZipFunctorOps[F[_],A](v: F[A])(implicit F0: ZipFunctor[F]) =
    new ZipFunctorOps[F,A] { def self = v; implicit def F: ZipFunctor[F] = F0 }

  ////

  ////
}

trait ZipFunctorSyntax[F[_]] extends FunctorSyntax[F] with ZipSyntax[F] {
  implicit def ToZipFunctorOps[A](v: F[A]): ZipFunctorOps[F, A] = new ZipFunctorOps[F,A] { def self = v; implicit def F: ZipFunctor[F] = ZipFunctorSyntax.this.F }

  def F: ZipFunctor[F]
  ////

  ////
}
