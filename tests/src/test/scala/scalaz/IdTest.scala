package scalaz

import std.anyVal._
import Id._

object IdTest extends Scalaprops {
  val testLaws = Properties.list(
    laws.monad.all[Id],
    laws.traverse.all[Id],
    laws.zip.all[Id],
    laws.align.all[Id],
    laws.comonad.all[Id]
  )
}
