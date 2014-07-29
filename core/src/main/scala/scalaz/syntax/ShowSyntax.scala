package scalaz
package syntax

import spire.macrosk.Ops
import scala.language.experimental.macros

/** Wraps a value `lhs` and provides methods related to `Show` */
final class ShowOps[F] private[syntax](lhs: F)(implicit val F: Show[F]) {
  ////
  final def show: Cord = F.show(lhs)
  final def shows: String = F.shows(lhs)
  final def print: Unit = Console.print(shows)
  final def println: Unit = Console.println(shows)
  ////
}

trait ToShowOps  {
  implicit def ToShowOps[F](v: F)(implicit F0: Show[F]) =
    new ShowOps[F](v)

  ////

  ////
}

trait ShowSyntax[F]  {
  implicit def ToShowOps(v: F): ShowOps[F] = new ShowOps[F](v)(ShowSyntax.this.F)

  def F: Show[F]
  ////

  ////
}
