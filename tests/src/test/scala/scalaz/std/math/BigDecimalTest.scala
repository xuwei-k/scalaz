package scalaz
package std
package math

import std.AllInstances._
import org.scalacheck.{Gen, Arbitrary}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import Tags.Multiplication
import _root_.java.math.MathContext._

object BigDecimalTest extends SpecLite {

  implicit val bigDecimalArb: Arbitrary[BigDecimal] =
    Arbitrary(
      Apply[Gen].apply3(
        Gen.choose(Long.MinValue, Long.MaxValue),
        Gen.choose(0, Long.MaxValue),
        Gen.oneOf(DECIMAL128, DECIMAL32, DECIMAL64, UNLIMITED)
      )((a, b, c) => BigDecimal(a.toString + "." + b.toString)(c))
    )

  implicit val bigDecimalMultiArb: Arbitrary[BigDecimal @@ Multiplication] =
    Multiplication.subst(bigDecimalArb)

  implicit val bigDecimalMultiEqual: Equal[BigDecimal @@ Multiplication] =
    Multiplication.subst(Equal[BigDecimal])

  checkAll("BigDecimal", enum.laws[BigDecimal])
  checkAll("BigDecimal", monoid.laws[BigDecimal])

  checkAll("BigDecimal @@ Multiplication", monoid.laws[BigDecimal @@ Multiplication])

}
