package scalaz

object OrderingTest extends Scalaprops {
  val testLaws = Properties.list(
    laws.enum.all[Ordering],
    laws.monoid.all[Ordering]
  )
}
