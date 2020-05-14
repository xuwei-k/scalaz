package scalaz.example

import reflect.ClassTag

object STUsage extends App {
  import scalaz._
  import std.list._
  import std.string._
  import std.anyVal._
  import syntax.equal._
  import effect._
  import ST._

  // Creates a new mutable reference and mutates it
  def e1[A] = for {
    r <- newVar[A](0)
    x <- r.mod(_ + 1)
  } yield x

  // Creates a new mutable reference, mutates it, and reads its value.
  def e2[A] = e1[A].flatMap(_.read)

  // Run e2, returning the final value of the mutable reference.
  def test = new Forall[ST[*,Int]] {
    def apply[A] = e2
  }

  // Run e1, returning a mutable reference to the outside world.
  // The type system ensures that this can never be run.
  def test2 = new Forall[λ[S=>ST[S,STRef[S,Int]]]] {
    def apply[A] = e1
  }

  val compiles = runST(test)


  assert(compiles === 1)
}
