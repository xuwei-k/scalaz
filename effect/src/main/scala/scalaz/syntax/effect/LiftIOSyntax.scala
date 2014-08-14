package scalaz
package syntax
package effect

import scalaz.effect.LiftIO

import machinist.DefaultOps
import scala.language.experimental.macros

/** Wraps a value `self` and provides methods related to `LiftIO` */
final class LiftIOOps[F[_],A] private[syntax](val self: F[A])(implicit val F: LiftIO[F]) {
  ////
  
  ////
}

sealed trait ToLiftIOOps0 {
  implicit def ToLiftIOOpsUnapply[FA](v: FA)(implicit F0: Unapply[LiftIO, FA]) =
    new LiftIOOps[F0.M,F0.A](F0(v))(F0.TC)

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
