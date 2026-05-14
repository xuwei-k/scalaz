package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `EqualIsNatural` */
final class EqualIsNaturalOps[F] private[syntax](val self: F)(implicit val F: EqualIsNatural[F]) extends Ops[F] {
  ////

  ////
}

trait ToEqualIsNaturalOps extends ToEqualOps {
  implicit def ToEqualIsNaturalOps[F](v: F)(implicit F0: EqualIsNatural[F]): EqualIsNaturalOps[F] =
    new EqualIsNaturalOps[F](v)

  ////

  ////
}

trait EqualIsNaturalSyntax[F] extends EqualSyntax[F] {
  implicit def ToEqualIsNaturalOps(v: F): EqualIsNaturalOps[F] = new EqualIsNaturalOps[F](v)(using EqualIsNaturalSyntax.this.F)

  def F: EqualIsNatural[F]
  ////

  ////
}
