package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Divide` */
final class DivideOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Divide[F]) extends Ops[F[A]] {
  ////

  ////
}

sealed trait ToDivideOps0 {


}

trait ToDivideOps extends ToDivideOps0 with ToContravariantOps {
  implicit def ToDivideOps[F[_],A](v: F[A])(implicit F0: Divide[F]) =
    new DivideOps[F,A](v)

  ////

  ////
}

trait DivideSyntax[F[_]] extends ContravariantSyntax[F] {
  implicit def ToDivideOps[A](v: F[A]): DivideOps[F, A] = new DivideOps[F,A](v)(DivideSyntax.this.F)

  def F: Divide[F]
  ////

  ////
}
