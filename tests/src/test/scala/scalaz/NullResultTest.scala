package scalaz

import org.scalacheck.Arbitrary
import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import NullResult._

class NullResultTest extends Spec{

  type NullResultInt[α] = NullResult[Int, α]

  implicit def EqualNullResult = Equal.equalBy[Int =>? Int, Option[Int]](_.apply(0))

  implicit def NullResultArb[A, B](implicit A: Arbitrary[Function1[A, B]]): Arbitrary[A =>? B] =
    Functor[Arbitrary].map(A)(NullResult.lift)
  
  checkAll(monoid.laws[NullResultInt[Int]])
//  checkAll(contravariant.laws[NullResultInt]) // TODO
  checkAll(monad.laws[NullResultInt])
  checkAll(category.laws[NullResult])

}
