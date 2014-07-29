package scalaz
package syntax

import spire.macrosk.Ops
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Equal` */
final class EqualOps[F] private[syntax](lhs: F)(implicit val F: Equal[F]) {
  ////

  final def ===(other: F): Boolean = F.equal(lhs, other)
  final def /==(other: F): Boolean = !F.equal(lhs, other)
  final def =/=(other: F): Boolean = /==(other)
  final def ≟(other: F): Boolean = F.equal(lhs, other)
  final def ≠(other: F): Boolean = !F.equal(lhs, other)

  /** Raises an exception unless lhs === other. */
  final def assert_===[B](other: B)(implicit S: Show[F], ev: B <:< F) =
      if (/==(other)) sys.error(S.shows(lhs) + " ≠ " + S.shows(ev(other)))

  ////
}

trait ToEqualOps  {
  implicit def ToEqualOps[F](v: F)(implicit F0: Equal[F]) =
    new EqualOps[F](v)

  ////

  ////
}

trait EqualSyntax[F]  {
  implicit def ToEqualOps(v: F): EqualOps[F] = new EqualOps[F](v)(EqualSyntax.this.F)

  def F: Equal[F]
  ////

  ////
}
