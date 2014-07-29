package scalaz
package syntax

import spire.macrosk.Ops
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Enum` */
final class EnumOps[F] private[syntax](lhs: F)(implicit val F: Enum[F]) {
  ////
  final def succ: F =
    F succ lhs

  final def -+-(n: Int): F =
    F.succn(n, lhs)

  final def succx: Option[F] =
    F.succx.apply(lhs)

  final def pred: F =
    F pred lhs

  final def ---(n: Int): F =
    F.predn(n, lhs)

  final def predx: Option[F] =
    F.predx.apply(lhs)

  final def from: EphemeralStream[F] =
    F.from(lhs)

  final def fromStep(step: Int): EphemeralStream[F] =
    F.fromStep(step, lhs)

  final def |=>(to: F): EphemeralStream[F] =
    F.fromTo(lhs, to)

  final def |->(to: F): List[F] =
    F.fromToL(lhs, to)

  final def |==>(step: Int, to: F): EphemeralStream[F] =
    F.fromStepTo(step, lhs, to)

  final def |-->(step: Int, to: F): List[F] =
    F.fromStepToL(step, lhs, to)

  ////
}

trait ToEnumOps extends ToOrderOps {
  implicit def ToEnumOps[F](v: F)(implicit F0: Enum[F]) =
    new EnumOps[F](v)

  ////

  ////
}

trait EnumSyntax[F] extends OrderSyntax[F] {
  implicit def ToEnumOps(v: F): EnumOps[F] = new EnumOps[F](v)(EnumSyntax.this.F)

  def F: Enum[F]
  ////

  ////
}
