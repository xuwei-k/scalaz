package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.Prop.forAll

object IdTest extends SpecLite {
  checkAll(monad.laws[Id])
  checkAll(traverse.laws[Id])
  checkAll(zip.laws[Id])
  checkAll(align.laws[Id])
  checkAll(comonad.laws[Id])
}
