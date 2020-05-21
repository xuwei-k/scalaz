package scalaz

import std.AllInstances._
import org.scalacheck.Prop.forAll

class BooleanSyntaxTest2 extends SpecLite {
  "boolean syntax" in {
    import syntax.std.boolean._

    "boolean.whenMU" ! forAll { (b: Boolean) =>
      import syntax.validation._
      b.whenMU("false".failure).isSuccess != b
    }

    "boolean.unlessMU" ! forAll { (b: Boolean) =>
      import syntax.validation._
      b.unlessMU("false".failure).isSuccess == b
    }
  }
}
