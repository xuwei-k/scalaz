package scalaz

trait SpecLitePlatform {
  def updateName: Unit = {
    val f = classOf[org.scalacheck.Properties].getDeclaredField("name")
    f.setAccessible(true)
    f.set(this, getClass.getName.stripSuffix("$"))
  }
}
