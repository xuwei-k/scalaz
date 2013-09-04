package scalaz

import std.AllInstances._
import syntax.foldable._
import scalaz.scalacheck.ScalazArbitrary._

class PLensTest extends Spec {
  import PLens._

  "list head" in {
    listHeadPLens[Int].get(List(1, 2)) must be_===(Some(1))
    listHeadPLens[Int].get(Nil) must be_===(None)
  }

  "nth PLens" in {
    def check[F[_]: Foldable](l: F[Int] @?> Int, xs: F[Int], x: Int, n: Int) = {
      val setAndGet = l.setK(xs) >=> l.getK run x
      l.get(xs) must be_===(xs.index(n))
      if(xs.index(n).isDefined && n >= 0)
        setAndGet must be_===(Some(x))
      else
        setAndGet must be_===(None)
    }

    "List" ! prop { (xs: List[Int], x: Int, n: Int) =>
      check(listNthPLens(n), xs, x, n)
    }

    "Stream" ! prop { (xs: Stream[Int], x: Int, n: Int) =>
      check(streamNthPLens(n), xs, x, n)
    }

    "EphemeralStream" ! prop { (xs: EphemeralStream[Int], x: Int) =>
      val n = util.Random.nextInt(100) // TODO avoid stack overflow
      check(ephemeralStreamNthPLens(n), xs, x, n)
    }

    "Vector" ! prop { (xs: List[Int], x: Int, n: Int) =>
      check[Vector](vectorNthPLens(n), xs.to, x, n)
    }

    "infinite Stream" in {
      val n, x = util.Random.nextInt(1000)
      check(streamNthPLens(n), Stream from 0, x, n)
    }

/*
    "infinite EphemeralStream" in {
      val n, x = util.Random.nextInt(100)
      check(ephemeralStreamNthPLens(n), EphemeralStream.iterate(0)(_ + 1), x, n)
    }
*/
  }

  object instances {
    def category = Category[PLens]
    def choice = Choice[PLens]
    def split = Split[PLens]
  }

}
