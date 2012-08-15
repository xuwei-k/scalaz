//import com.jsuereth.pgp.sbtplugin.PgpKeys._

crossScalaVersions := Seq("2.9.1", "2.9.2", "2.10.0-M5")

credentials := Seq(Credentials(Path.userHome / ".ivy2" / ".rgcredentials"))

publishTo <<= (version) { version: String =>
  val nexus = "http://nexus.reportgrid.com/content/repositories/"
  if (version.trim.endsWith("SNAPSHOT")) Some("snapshots" at nexus+"public-snapshots/") 
  else                                   Some("releases"  at nexus+"public-releases/")
}

//skip in pgpSigner := true
