package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cozip` */
final class CozipOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Cozip[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToCozipOps0 {


}

trait ToCozipOps extends ToCozipOps0 {
  implicit def ToCozipOps[F[_],A](v: F[A])(implicit F0: Cozip[F]) =
    new CozipOps[F,A](v)

  ////

  ////
}

trait CozipSyntax[F[_]]  {
  implicit def ToCozipOps[A](v: F[A]): CozipOps[F, A] = new CozipOps[F,A](v)(CozipSyntax.this.F)

  def F: Cozip[F]
  ////

  ////
}
