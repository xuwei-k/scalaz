import build._

import com.typesafe.sbt.osgi.OsgiKeys
import com.typesafe.tools.mima.plugin.MimaKeys.mimaPreviousArtifacts
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM_1_12, scalacheckBindingJVM_1_13, testsJVM, concurrent, example
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, nativeTest
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ Seq[Sett](
    mimaPreviousArtifacts := Set.empty,
    description := "scalaz unidoc",
    artifacts := Classpaths.artifactDefs(Seq(packageDoc in Compile, makePom in Compile)).value,
    packagedArtifacts := Classpaths.packaged(Seq(packageDoc in Compile, makePom in Compile)).value,
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      val rule = new RewriteRule {
        override def transform(n: Node) =
          if (n.label == "dependencies") NodeSeq.Empty else n
      }
      new RuleTransformer(rule).transform(node)(0)
    },
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a)) -- inProjects(scalacheckBindingJVM_1_12)
    }
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = jvmProjects ++ jsProjects
).enablePlugins(ScalaUnidocPlugin)

lazy val rootNative = Project(
  rootNativeId,
  file("rootNative")
).settings(
  standardSettings,
  notPublish
).aggregate(nativeProjects: _*)

lazy val rootJS = Project(
  "rootJS",
  file("rootJS")
).settings(
  standardSettings,
  notPublish,
  mimaPreviousArtifacts := Set.empty
).aggregate(jsProjects: _*)

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  notPublish,
  mimaPreviousArtifacts := Set.empty
).aggregate(jvmProjects: _*)

lazy val coreJVM = core.jvm
lazy val coreJS  = core.js
lazy val coreNative = core.native

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
)

lazy val effectJVM = effect.jvm
lazy val effectJS  = effect.js
lazy val effectNative = effect.native

lazy val iterateeJVM = iteratee.jvm
lazy val iterateeJS  = iteratee.js
lazy val iterateeNative = iteratee.native

lazy val example = Project(
  id = "example",
  base = file("example"),
  dependencies = Seq(coreJVM, iterateeJVM, concurrent),
  settings = standardSettings ++ Seq[Sett](
    name := "scalaz-example",
    mimaPreviousArtifacts := Set.empty,
    publishArtifact := false
  )
)

def scalacheckBindingProject(id: String, base: String, scalacheckVersion: SettingKey[String]) =
  sbtcrossproject.CrossProject(id, file(base), ScalazCrossType, JVMPlatform, JSPlatform)
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/src/main/scala"
      },
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalacheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings)
    .jvmSettings(
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/jvm/src/main/scala"
      }
    )
    .jsSettings(
      (unmanagedSourceDirectories in Compile) += {
        (baseDirectory in LocalRootProject).value / "scalacheck-binding/js/src/main/scala"
      }
    )

lazy val scalacheckBinding_1_12 =
  scalacheckBindingProject("scalacheck-binding_1_12", "scalacheck-binding_1_12", scalaCheckVersion_1_12)
lazy val scalacheckBindingJVM_1_12 = scalacheckBinding_1_12.jvm


lazy val scalacheckBinding_1_13 = {
  def scalacheckBinding_1_13Version(base: String) = base + "-scalacheck-1.13"

  scalacheckBindingProject("scalacheck-binding_1_13", "scalacheck-binding_1_13", scalaCheckVersion_1_13).settings(
    version ~= { v =>
      val snapshotSuffix = "-SNAPSHOT"
      if(v.endsWith(snapshotSuffix)) {
        scalacheckBinding_1_13Version(v.dropRight(snapshotSuffix.length)) + snapshotSuffix
      } else {
        scalacheckBinding_1_13Version(v)
      }
    },
    mimaPreviousArtifacts := {
      Set.empty
    }
  )
}

lazy val scalacheckBindingJVM_1_13 = scalacheckBinding_1_13.jvm


lazy val tests = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    mimaPreviousArtifacts := Set.empty,
    publishArtifact := false
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding_1_13)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings)

lazy val testsJVM = tests.jvm

// can't use "sbt test"
// https://github.com/scala-native/scala-native/issues/339
lazy val nativeTest = Project(nativeTestId, file("nativeTest")).enablePlugins(ScalaNativePlugin)
  .settings(
    standardSettings,
    nativeSettings,
    notPublish
  )
  .dependsOn(iterateeNative)
