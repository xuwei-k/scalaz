package scalaz

import org.scalacheck._
import org.scalacheck.Prop.Result
import org.scalacheck.Gen.Parameters

object SpecLite {
  import scala.collection.mutable

  private case class CurrentTest(name: String, startTime: Long)
  @volatile private var current: Option[CurrentTest] = None
  private val m = new mutable.HashMap[String, Long]()

  def printTestTimes(): Unit = {
    println()
    m.toList.sortBy(_._2).foreach{ case (n, t) => println((t / 1000000.0, n)) }
    println()
    m.clear()
  }

  private def change(test: SpecLite){
    val name = test.getClass.getName
    synchronized{
      current.map{ pre =>
        val t = System.nanoTime - pre.startTime
        m += ((pre.name, t))
        val time = t / 1000000.0
        println("\n ------ stop ------ " + pre.name + " " + time + "\n")
      }.getOrElse{
        m.clear()
      }
      current = Some(CurrentTest(name, System.nanoTime))
    }
    println("\n ------ start ------ " + name + "\n")
  }
}

abstract class SpecLite extends Properties("") {
  def updateName: Unit = {
    val f = classOf[Properties].getDeclaredField("name")
    f.setAccessible(true)
    f.set(this, getClass.getName.stripSuffix("$"))
  }
  updateName

  def maxSize: Option[Int] = None

  SpecLite.change(this)

  private def contramapProp(prop: Prop)(f: Parameters => Parameters): Prop =
    Prop(params => prop(f(params)))

  private def resizeProp(name: String, prop: Prop): Prop = {
    if(name contains "sequential fusion") {
      prop
    }else{
      maxSize.map(max =>
        contramapProp(prop)(_.withSize(scala.util.Random.nextInt(max)))
      ).getOrElse(prop)
    }
  }

  override def properties: Seq[(String, Prop)] =
    super.properties.map{ case (name, prop) =>
      name -> resizeProp(name, prop)
    }

  def checkAll(name: String, props: Properties) {
    for ((name2, prop) <- props.properties) yield {
      property(name + ":" + name2) = resizeProp(name, prop)
    }
  }

  def checkAll(props: Properties) {
    for ((name, prop) <- props.properties) yield {
      property(name) = resizeProp(name, prop)
    }
  }

  class PropertyOps(props: Properties) {
    def withProp(propName: String, prop: Prop) = new Properties(props.name) {
      for {(name, p) <- props.properties} property(name) = resizeProp(name, p)
      property(propName) = resizeProp(propName, prop)
    }
  }

  implicit def enrichProperties(props: Properties) = new PropertyOps(props)
  private var context: String = ""

  class StringOps(s: String) {
    def should[A](a: => Any): Unit = {
      val saved = context
      context = s; try a finally context = saved
    }
    def ![A](a: => A)(implicit ev: (A) => Prop): Unit = in(a)
    def in[A](a: => A)(implicit ev: (A) => Prop): Unit = property(context + ":" + s) = new Prop {
      def apply(prms: Parameters): Result = ev(a).apply(prms) // TODO sort out the laziness / implicit conversions properly
    }
  }

  implicit def enrichString(s: String) = new StringOps(s)

  def check(x: => Boolean): Prop = {
    x must_==(true)
  }

  def fail(msg: String): Nothing = throw new AssertionError(msg)
  class AnyOps[A](actual: => A) {
    def must_===(expected: A)(implicit show: Show[A], equal: Equal[A]): Unit = {
      val act = actual
      def test = Equal[A].equal(expected, act)
      def koMessage = "%s !== %s".format(Show[A].shows(act), Show[A].shows(expected))
      if (!test)
        fail(koMessage)
    }
    def must_==(expected: A): Unit = {
      val act = actual
      def test = expected == act
      def koMessage = "%s !== %s".format(act, expected)
      if (!test)
        fail(koMessage)
    }

    def mustMatch(f: PartialFunction[A, Boolean]): Unit = {
      val act = actual
      def test = f.isDefinedAt(act) && f(act)
      def koMessage = "%s does not satisfy partial function".format(act)
      if (!test)
        fail(koMessage)
    }

    def and[B](b: => B): B = {
      actual
      b
    }

    def mustBe_<(x: Int)(implicit ev: A <:< Int) = {
      val act = actual
      def test = ev(act) < x
      def koMessage = "%s <! %s".format(actual, x)
      if (!test)
        fail(koMessage)
    }

    def mustThrowA[T <: Throwable](implicit man: ClassManifest[T]): Unit = {
      val erasedClass = man.erasure
      try {
        actual
        fail("no exception thrown, expected " + erasedClass)
      } catch {
        case ex: Throwable =>
          if (!erasedClass.isInstance(ex))
            fail("wrong exception thrown, expected: " + erasedClass + " got: " + ex)
      }
    }
  }
  implicit def enrichAny[A](actual: => A): AnyOps[A] = new AnyOps(actual)

  def prop[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = check1(result)
  implicit def propToProp(p: => Prop): Prop = p
  implicit def check1[T, R](result: T => R)(implicit toProp: (=>R) => Prop, a: Arbitrary[T], s: Shrink[T]): Prop = Prop.forAll((t: T) => toProp(result(t)))
  implicit def unitToProp(u: => Unit): Prop = booleanToProp({u; true})
  implicit def unitToProp2(u: Unit): Prop = booleanToProp(true)
  //implicit def unitToProp2(u: Unit): Prop = booleanToProp(true)
  implicit def booleanToProp(b: => Boolean): Prop = Prop.secure(b)
//    implicit def callByNameMatchResultToProp[T](m: =>MatchResult[T]): Prop = resultProp(m.toResult)
//    implicit def matchResultToProp[T](m: MatchResult[T]): Prop = resultProp(m.toResult)

  /**
   * Most of our scalacheck tests use (Int => Int). This generator includes non-constant
   * functions (id, inc), to have a better chance at catching bugs.
   */
  implicit def Function1IntInt[A](implicit A: Arbitrary[Int]): Arbitrary[Int => Int] =
    Arbitrary(Gen.frequency[Int => Int](
      (1, Gen.value((x: Int) => x)),
      (1, Gen.value((x: Int) => x + 1)),
      (3, A.arbitrary.map(a => (_: Int) => a))
    ))
}
