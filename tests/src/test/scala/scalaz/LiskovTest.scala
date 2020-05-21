package scalaz

class LiskovTest extends SpecLite {

  trait Co1[+A]

  trait Contra1[-A]

  trait Co1_2[+A, B]

  trait Co2_2[A, +B]

  trait Contra1_2[-A, B]

  trait Contra2_2[A, -B]

  import Liskov._

  "apply" in {
    implicitly[String <:< AnyRef].apply(""): AnyRef
    ()
  }

  "lift" in {
    def foo[A, B](implicit ev: A <~< B): Unit = {
      Liskov.co[Co1, A, B](ev)
      Liskov.contra[Contra1, A, B](ev)

      Liskov.co2[Co1_2, B, A, Unit](ev)
      Liskov.co2_2[Co2_2, B, Unit, A](ev)
      Liskov.contra1_2[Contra1_2, B, A, Unit](ev)
      Liskov.contra2_2[Contra2_2, B, Unit, A](ev)
    }

    foo[String, AnyRef]

  }
}
