package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ZipUnzip` */
trait ZipUnzipOps[F[_],A] extends Ops[F[A]] {
  implicit def F: ZipUnzip[F]
  ////

  ////
}

trait ToZipUnzipOps0 {
  implicit def ToZipUnzipOpsUnapply[FA](v: FA)(implicit F0: Unapply[ZipUnzip, FA]) =
    new ZipUnzipOps[F0.M,F0.A] { def self = F0(v); implicit def F: ZipUnzip[F0.M] = F0.TC }

}

trait ToZipUnzipOps extends ToZipUnzipOps0 with ToZipFunctorOps with ToUnzipOps {
  implicit def ToZipUnzipOps[F[_],A](v: F[A])(implicit F0: ZipUnzip[F]) =
    new ZipUnzipOps[F,A] { def self = v; implicit def F: ZipUnzip[F] = F0 }

  ////

  ////
}

trait ZipUnzipSyntax[F[_]] extends ZipFunctorSyntax[F] with UnzipSyntax[F] {
  implicit def ToZipUnzipOps[A](v: F[A]): ZipUnzipOps[F, A] = new ZipUnzipOps[F,A] { def self = v; implicit def F: ZipUnzip[F] = ZipUnzipSyntax.this.F }

  def F: ZipUnzip[F]
  ////

  ////
}
