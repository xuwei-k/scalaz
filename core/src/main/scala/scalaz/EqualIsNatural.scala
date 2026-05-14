package scalaz

////
/**
 *
 */
////
trait EqualIsNatural[F] extends Equal[F] { self =>
  ////

  trait EqualIsNaturalLaw extends EqualLaw{
    def naturality(f1: F, f2: F): Boolean =
      equal(f1, f2) == (f1 == f2)
  }
  def equalIsNaturalLaw: EqualLaw = new EqualLaw {}

  ////
  val equalIsNaturalSyntax: scalaz.syntax.EqualIsNaturalSyntax[F] =
    new scalaz.syntax.EqualIsNaturalSyntax[F] { def F = EqualIsNatural.this }
}

object EqualIsNatural {
  @inline def apply[F](implicit F: EqualIsNatural[F]): EqualIsNatural[F] = F

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: EqualIsNatural[G]): EqualIsNatural[F] =
    new IsomorphismEqualIsNatural[F, G] {
      override def G: EqualIsNatural[G] = M
      override def iso: F <=> G = D
    }

  ////

  ////
}

trait IsomorphismEqualIsNatural[F, G] extends EqualIsNatural[F] with IsomorphismEqual[F, G]{
  implicit def G: EqualIsNatural[G]
  ////

  ////
}
