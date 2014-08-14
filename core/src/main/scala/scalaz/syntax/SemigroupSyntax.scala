package scalaz
package syntax

import machinist.DefaultOps
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Semigroup` */
final class SemigroupOps[F] private[syntax](lhs: F)(implicit val F: Semigroup[F]) {
  ////
  final def |+|(other: => F): F = F.append(lhs, other)
  final def mappend(other: => F): F = F.append(lhs, other)
  final def ⊹(rhs: => F): F = macro DefaultOps.binop[F, F]
  ////
}

trait ToSemigroupOps  {
  implicit def ToSemigroupOps[F](v: F)(implicit F0: Semigroup[F]) =
    new SemigroupOps[F](v)

  ////
  ////
}

trait SemigroupSyntax[F]  {
  implicit def ToSemigroupOps(v: F): SemigroupOps[F] = new SemigroupOps[F](v)(SemigroupSyntax.this.F)

  def F: Semigroup[F]
  ////
  def mappend(f1: F, f2: => F)(implicit F: Semigroup[F]): F = F.append(f1, f2)

  ////
}
