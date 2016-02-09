package scalaz

object EndoTest extends Scalaprops {

  implicit val endoIntEqual: Equal[Endo[Int]] =
    Equal.equal( (a, b) =>
      Iterator.fill(20)(util.Random.nextInt).forall(n => a(n) == b(n))
    )

  val testLaws = laws.invariantFunctor.all[Endo]

}
