package scalaz

import Property.forAll

object BooleanTest extends Scalaprops {
  import scalaz.std.{boolean => b}

  val and = forAll { (p:Boolean, q:Boolean) =>
    b.conjunction(p, q) == (p && q)
  }

  val or = forAll { (p:Boolean, q:Boolean) =>
    b.disjunction(p, q) == (p || q)
  }

  val nand = forAll { (p:Boolean, q:Boolean) =>
    b.nand(p, q) == !(p && q)
  }

  val nor = forAll { (p:Boolean, q:Boolean) =>
    b.nor(p, q) == !(p || q)
  }

  val conditional = forAll { (p:Boolean, q:Boolean) =>
    b.conditional(p, q)  == (!p || q)
  }

  val `inverse conditional` = forAll { (p:Boolean, q:Boolean) =>
    b.inverseConditional(p, q)  == (p || !q)
  }

  val `negate conditional` = forAll { (p:Boolean, q:Boolean) =>
    b.negConditional(p, q)  == (p && !q)
  }

  val `negate inverse conditional` = forAll { (p:Boolean, q:Boolean) =>
    b.negInverseConditional(p, q) == (!p && q)
  }
}
