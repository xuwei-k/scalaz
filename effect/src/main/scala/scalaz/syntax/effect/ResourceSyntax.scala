package scalaz
package syntax
package effect

import scalaz.effect.Resource

import spire.macrosk.Ops
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Resource` */
final class ResourceOps[F] private[syntax](lhs: F)(implicit val F: Resource[F]) {
  ////

  def close = F.close(lhs)

  ////
}

trait ToResourceOps  {
  implicit def ToResourceOps[F](v: F)(implicit F0: Resource[F]) =
    new ResourceOps[F](v)

  ////

  ////
}

trait ResourceSyntax[F]  {
  implicit def ToResourceOps(v: F): ResourceOps[F] = new ResourceOps[F](v)(ResourceSyntax.this.F)

  def F: Resource[F]
  ////

  ////
}
