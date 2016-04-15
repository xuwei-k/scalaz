package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Catchable` */
final class CatchableOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Catchable[F]) extends Ops[F[A]] {
  ////
  def attempt: F[Throwable \/ A] = F.attempt(self)
  ////
}

sealed trait ToCatchableOps0 {


}

trait ToCatchableOps extends ToCatchableOps0 {
  implicit def ToCatchableOps[F[_],A](v: F[A])(implicit F0: Catchable[F]) =
    new CatchableOps[F,A](v)

  ////

  ////
}

trait CatchableSyntax[F[_]]  {
  implicit def ToCatchableOps[A](v: F[A]): CatchableOps[F, A] = new CatchableOps[F,A](v)(CatchableSyntax.this.F)

  def F: Catchable[F]
  ////

  ////
}
