package scalaz
package laws

import scalaz.Property.forAll
import scalaz.Properties.properties

object semigroup {
  def associative[A: Equal: Gen](implicit A: Semigroup[A]): Property =
    forAll(A.semigroupLaw.associative _)

  def laws[A: Semigroup: Equal: Gen]: Properties[ScalazLaw] =
    properties(ScalazLaw.semigroup) (
      ScalazLaw.semigroupAssociative -> associative[A]
    )

  def all[A: Semigroup: Equal: Gen]: Properties[ScalazLaw] =
    laws[A]
}
