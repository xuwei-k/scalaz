package scalaz
package std
package math

import _root_.java.math.MathContext.UNLIMITED
import _root_.java.math.{BigDecimal => JavaBigDecimal}

trait BigDecimalInstances {
  implicit val bigDecimalInstance: Monoid[BigDecimal] with Enum[BigDecimal] with Show[BigDecimal] = new Monoid[BigDecimal] with Enum[BigDecimal] with Show[BigDecimal] {
    override def shows(f: BigDecimal) = f.toString

    def append(f1: BigDecimal, f2: => BigDecimal) = f1.bigDecimal.add(f2.bigDecimal, UNLIMITED)

    def zero = 0L

    def order(x: BigDecimal, y: BigDecimal) = if (x < y) Ordering.LT else if (x == y) Ordering.EQ else Ordering.GT

    def succ(b: BigDecimal) = b.bigDecimal.add(JavaBigDecimal.valueOf(1), UNLIMITED)
    def pred(b: BigDecimal) = b.bigDecimal.subtract(JavaBigDecimal.valueOf(1), UNLIMITED)
    override def succn(a: Int, b: BigDecimal) = b.bigDecimal.add(JavaBigDecimal.valueOf(a), UNLIMITED)
    override def predn(a: Int, b: BigDecimal) = b.bigDecimal.subtract(JavaBigDecimal.valueOf(a), UNLIMITED)
    override def min = None
    override def max = None
  }

  import Tags.Multiplication

  implicit val BigDecimalMultiplicationNewType: Monoid[BigDecimal @@ Multiplication] = new Monoid[BigDecimal @@ Multiplication] {
    def append(f1: BigDecimal @@ Multiplication, f2: => BigDecimal @@ Multiplication) =
      Multiplication(BigDecimal(f1.bigDecimal.multiply(f2.bigDecimal, UNLIMITED)))

    val zero = Multiplication(BigDecimal(1))
  }
}

object bigDecimal extends BigDecimalInstances
