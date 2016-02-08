package scalaz

object DigitTest extends Scalaprops{
  val testLaws = Properties.list(
    laws.enum.all[Digit],
    laws.monoid.all[Digit]
  )
}
