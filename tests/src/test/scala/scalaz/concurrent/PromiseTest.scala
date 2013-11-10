package scalaz
package concurrent

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop._
import ConcurrentTest._

class PromiseTest extends Spec {
  implicit def promiseEqual[A: Equal] = Equal[A].contramap((_: Promise[A]).get)

  checkAll(monad.laws[Promise])
  checkAll(traverse.laws[Promise])
  checkAll(comonad.laws[Promise])

  check(throws(classOf[Error])(Promise({throw new Error("x"); 1}).flatMap(_ => Promise(2)).get))
  check(throws(classOf[Promise.BrokenException])(Promise(0).filter(_ != 0).get))
  check(forAll((x: Int) => Promise(x).filter(_ => true).get === x))

  class OhNo extends RuntimeException("OhNo!")

  "Promise" should {
    import Scalaz._
    "not hang when an error occurs in sequence" in {
      withTimeout(2000) {
        throws(classOf[OhNo])(List(Promise({ throw new OhNo(); 1 })).sequence.get)
      }
    }
    "not hang when an error occurs on filter" in {
      withTimeout(2000) {
        throws(classOf[OhNo])(Promise({ throw new OhNo(); 2 }).filter(_ > 2).get)
      }
    }
  }

}
