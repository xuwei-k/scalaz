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
  def mustMatch(f: PartialFunction[A, Boolean]): Unit = {
    val act = actual
    def test = f.isDefinedAt(act) && f(act)
    def koMessage = "%s does not satisfy partial function".format(act)
    if (!test)
      throw new AssertionError(koMessage)
  }
  def mustThrowA[T <: Throwable: reflect.ClassTag](implicit man: reflect.ClassTag[T]): Unit = {
    val erasedClass = man.runtimeClass
    try {
      actual
      throw new AssertionError("no exception thrown, expected " + erasedClass)
    } catch {
      case ex: Throwable =>
        if (!erasedClass.isInstance(ex))
          throw new AssertionError("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
    }
  }
}
