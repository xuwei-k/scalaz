package scalaz
package std

trait TypeConstraintInstances {
  val <:< : Category[<:<] = new AbstractCategory[<:<] {
    def id[A] = conforms[A]
    def compose[A, B, C](f: B <:< C, g: A <:< B) = f.asInstanceOf[A <:< C]
  }

  val =:= : Category[=:=] = new AbstractCategory[=:=] {
    def id[A] = implicitly
    def compose[A, B, C](f: B =:= C, g: A =:= B) = f.asInstanceOf[A =:= C]
  }
}

object typeConstraint extends TypeConstraintInstances
