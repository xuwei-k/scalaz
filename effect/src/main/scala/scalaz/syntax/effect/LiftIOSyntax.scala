package scalaz
package syntax
package effect

import scalaz.effect.LiftIO

/** Wraps a value `self` and provides methods related to `LiftIO` */
final class LiftIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: LiftIO[F]) extends Ops[F[A]] {
  ////
  
  ////
}

sealed trait ToLiftIOOps0 {


}

trait ToLiftIOOps extends ToLiftIOOps0 {
  implicit def ToLiftIOOps[F[_],A](v: F[A])(implicit F0: LiftIO[F]) =
    new LiftIOOps[F,A](v)

  ////

  ////
}

trait LiftIOSyntax[F[_]]  {
  implicit def ToLiftIOOps[A](v: F[A]): LiftIOOps[F, A] = new LiftIOOps[F,A](v)(LiftIOSyntax.this.F)

  def F: LiftIO[F]
  ////

  ////
}
