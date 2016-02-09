package scalaz

final class AnyOps[A](actual: => A) {
  def must_===(expected: A)(implicit S: Show[A], E: Equal[A]): Unit = {
    val act = actual
    def test = E.equal(expected, act)
    def koMessage = "%s !== %s".format(Show[A].shows(act), Show[A].shows(expected))
    if (!test)
      throw new AssertionError(koMessage)
  }
  def must_==(expected: A): Unit = {
    val act = actual
    def test = expected == act
    def koMessage = "%s !== %s".format(act, expected)
    if (!test)
      throw new AssertionError(koMessage)
  }
}
