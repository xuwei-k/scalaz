package scalaz

import sbt.testing.SubclassFingerprint

private[scalaz] object ScalapropsFingerprint extends SubclassFingerprint {
  override def isModule: Boolean = true
  override def superclassName(): String = "scalaz.Scalaprops"
  override def requireNoArgConstructor(): Boolean = true
}
