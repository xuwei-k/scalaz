package scalaz
package laws

import scalaz.Property.forAll
import scalaz.std.anyVal._

object foldable {
  def leftFMConsistent[F[_], A](implicit F: Foldable[F], afa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldableLaw.leftFMConsistent[A] _)

  def rightFMConsistent[F[_], A](implicit F: Foldable[F], afa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldableLaw.rightFMConsistent[A] _)

  def laws[F[_]](implicit fa: Gen[F[Int]], F: Foldable[F]) =
    Properties.properties(ScalazLaw.foldable)(
      ScalazLaw.foldableLeftFMConsistent -> leftFMConsistent[F, Int],
      ScalazLaw.foldableRightFMConsistent -> rightFMConsistent[F, Int]
    )

  def all[F[_]](implicit fa: Gen[F[Int]], F: Foldable[F]) =
    laws[F]
}
