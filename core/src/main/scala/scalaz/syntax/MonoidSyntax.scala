package scalaz
package syntax

import machinist.DefaultOps
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Monoid` */
final class MonoidOps[F] private[syntax](lhs: F)(implicit val F: Monoid[F]) {
  ////
  final def multiply(rhs: Int): F = macro DefaultOps.binop[Int, F]

  final def ifEmpty[A](tv: => A)(fv: => A)(implicit e: Equal[F]): A = F.ifEmpty(lhs)(tv)(fv)

  final def isMZero(implicit rhs: Equal[F]): Boolean = macro DefaultOps.binop[Equal[F], Boolean]

  final def onNotEmpty[A](v: => A)(implicit ma: Monoid[A], e: Equal[F]): A = F.onNotEmpty(lhs)(v)

  final def onEmpty[A](v: => A)(implicit ma: Monoid[A], e: Equal[F]): A = F.onEmpty(lhs)(v)
  ////
}

trait ToMonoidOps extends ToSemigroupOps {
  implicit def ToMonoidOps[F](v: F)(implicit F0: Monoid[F]) =
    new MonoidOps[F](v)

  ////

  def mzero[F](implicit F: Monoid[F]): F = F.zero
  def ∅[F](implicit F: Monoid[F]): F = F.zero
  ////
}

trait MonoidSyntax[F] extends SemigroupSyntax[F] {
  implicit def ToMonoidOps(v: F): MonoidOps[F] = new MonoidOps[F](v)(MonoidSyntax.this.F)

  def F: Monoid[F]
  ////
  def mzero(implicit F: Monoid[F]): F = F.zero
  def ∅(implicit F: Monoid[F]): F = F.zero
  ////
}
