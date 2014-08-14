package scalaz
package syntax

import machinist.DefaultOps
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Order` */
final class OrderOps[F] private[syntax](lhs: F)(implicit val F: Order[F]) {
  ////
  final def <(other: F): Boolean = F.lessThan(lhs, other)
  final def <=(other: F): Boolean = F.lessThanOrEqual(lhs, other)
  final def >(other: F): Boolean = F.greaterThan(lhs, other)
  final def >=(other: F): Boolean = F.greaterThanOrEqual(lhs, other)
  final def max(other: F): F = F.max(lhs, other)
  final def min(other: F): F = F.min(lhs, other)
  final def cmp(other: F): Ordering = F.order(lhs, other)
  final def ?|?(other: F): Ordering = F.order(lhs, other)
  final def lte(other: F): Boolean = F.lessThanOrEqual(lhs, other)
  final def gte(other: F): Boolean = F.greaterThanOrEqual(lhs, other)
  final def lt(other: F): Boolean = F.lessThan(lhs, other)
  final def gt(other: F): Boolean = F.greaterThan(lhs, other)
  ////
}

trait ToOrderOps extends ToEqualOps {
  implicit def ToOrderOps[F](v: F)(implicit F0: Order[F]) =
    new OrderOps[F](v)

  ////

  ////
}

trait OrderSyntax[F] extends EqualSyntax[F] {
  implicit def ToOrderOps(v: F): OrderOps[F] = new OrderOps[F](v)(OrderSyntax.this.F)

  def F: Order[F]
  ////

  ////
}
