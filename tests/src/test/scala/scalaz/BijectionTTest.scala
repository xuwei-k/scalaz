package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object BijectionTTest extends Spec {

  checkAll("eitherB", bijection.laws(BijectionT.eitherB[Int]))
  checkAll("zipB", bijection.laws(BijectionT.zipB[List, Int, Int]))

  checkAll("tuple3B", bijection.laws(BijectionT.tuple3B[Int, Int, Int]))
  checkAll("tuple4B", bijection.laws(BijectionT.tuple4B[Int, Int, Int, Int]))
  checkAll("tuple5B", bijection.laws(BijectionT.tuple5B[Int, Int, Int, Int, Int]))
  checkAll("tuple6B", bijection.laws(BijectionT.tuple6B[Int, Int, Int, Int, Int, Int]))
  checkAll("tuple7B", bijection.laws(BijectionT.tuple7B[Int, Int, Int, Int, Int, Int, Int]))

}
