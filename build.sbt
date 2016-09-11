import build.{kindProjector => _, _}
import com.typesafe.sbt.osgi.OsgiKeys
import org.scalajs.sbtplugin.cross._
import sbtunidoc.Plugin.UnidocKeys._

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS, scalacheckBindingJS, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM, testsJVM, concurrent, example
)

lazy val kindProjector = build.kindProjector

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ unidocSettings ++ Seq[Sett](
    artifacts <<= Classpaths.artifactDefs(Seq(packageDoc in Compile)),
    packagedArtifacts <<= Classpaths.packaged(Seq(packageDoc in Compile)),
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      jsProjects.foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
    }
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = jvmProjects ++ jsProjects
).dependsOn(kindProjector % "plugin->default(compile)")

lazy val rootJS = Project(
  "rootJS",
  file("rootJS")
).settings(
  standardSettings,
  notPublish
).aggregate(jsProjects: _*).dependsOn(kindProjector % "plugin->default(compile)")

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  notPublish
).aggregate(jvmProjects: _*).dependsOn(kindProjector % "plugin->default(compile)")

lazy val coreJVM = core.jvm.dependsOn(kindProjector % "plugin->default(compile)")
lazy val coreJS  = core.js.dependsOn(kindProjector % "plugin->default(compile)")

lazy val concurrent = Project(
  id = "concurrent",
  base = file("concurrent"),
  settings = standardSettings ++ Seq(
    name := ConcurrentName,
    typeClasses := TypeClass.concurrent,
    osgiExport("scalaz.concurrent"),
    OsgiKeys.importPackage := Seq("javax.swing;resolution:=optional", "*")
  ),
  dependencies = Seq(coreJVM, effectJVM)
).dependsOn(kindProjector % "plugin->default(compile)")

lazy val effectJVM = effect.jvm.dependsOn(kindProjector % "plugin->default(compile)")
lazy val effectJS  = effect.js.dependsOn(kindProjector % "plugin->default(compile)")

lazy val iterateeJVM = iteratee.jvm.dependsOn(kindProjector % "plugin->default(compile)")
lazy val iterateeJS  = iteratee.js.dependsOn(kindProjector % "plugin->default(compile)")

lazy val example = Project(
  id = "example",
  base = file("example"),
  dependencies = Seq(coreJVM, iterateeJVM, concurrent),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-example",
    publishArtifact := false
  )
).dependsOn(kindProjector % "plugin->default(compile)")

lazy val scalacheckBinding =
  CrossProject("scalacheck-binding", file("scalacheck-binding"), ScalazCrossType)
    .settings(standardSettings: _*)
    .settings(
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings : _*)

lazy val scalacheckBindingJVM = scalacheckBinding.jvm.dependsOn(kindProjector % "plugin->default(compile)")
lazy val scalacheckBindingJS  = scalacheckBinding.js.dependsOn(kindProjector % "plugin->default(compile)")

lazy val tests = crossProject.crossType(ScalazCrossType)
  .settings(standardSettings: _*)
  .settings(
    name := "scalaz-tests",
    publishArtifact := false,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value % "test")
  .dependsOn(core, effect, iteratee, scalacheckBinding)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings : _*)
  .jsSettings(
    jsEnv := NodeJSEnv().value,
    scalaJSUseRhino in Global := false
  )

lazy val testsJVM = tests.jvm.dependsOn(kindProjector % "plugin->default(compile)")
lazy val testsJS  = tests.js.dependsOn(kindProjector % "plugin->default(compile)")
