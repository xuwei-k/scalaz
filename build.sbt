import build._
import com.typesafe.sbt.osgi.OsgiKeys
import org.scalajs.sbtplugin.cross._
import sbtunidoc.Plugin.UnidocKeys._
import sbtcrossproject.CrossPlugin.autoImport.crossProject

lazy val jsProjects = Seq[ProjectReference](
  coreJS, effectJS, iterateeJS, scalacheckBindingJS, testsJS
)

lazy val jvmProjects = Seq[ProjectReference](
  coreJVM, effectJVM, iterateeJVM, scalacheckBindingJVM, testsJVM, concurrent, example
)

lazy val nativeProjects = Seq[ProjectReference](
  coreNative, effectNative, iterateeNative, nativeTest
)

lazy val scalaz = Project(
  id = "scalaz",
  base = file("."),
  settings = standardSettings ++ unidocSettings ++ Seq[Sett](
    artifacts := Classpaths.artifactDefs(Seq(packageDoc in Compile)).value,
    packagedArtifacts := Classpaths.packaged(Seq(packageDoc in Compile)).value,
    unidocProjectFilter in (ScalaUnidoc, unidoc) := {
      (jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
    }
  ) ++ Defaults.packageTaskSettings(packageDoc in Compile, (unidoc in Compile).map(_.flatMap(Path.allSubpaths))),
  aggregate = jvmProjects ++ jsProjects
)

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
  notPublish
).aggregate(jsProjects: _*)

lazy val rootJVM = Project(
  "rootJVM",
  file("rootJVM")
).settings(
  standardSettings,
  notPublish
).aggregate(jvmProjects: _*)

def createJS(file: String, binary: String) = {
  val gunzip = IO.withTemporaryDirectory{ dir =>
    val js = dir / "gunzipjs"
    IO.download(url("https://raw.githubusercontent.com/imaya/zlib.js/b99bd33d485d63e93310b911a1f8dbf9ceb265d5/bin/gunzip.min.js"), js)
    IO.readLines(js).mkString("\n").replace("//@ sourceMappingURL=gunzip.min.js.map", "")
  }

  gunzip + s"""
  var str = "$binary";
  var array = new Uint8Array(str.length / 2);
  var len = str.length * 2;
  for(var i = 0; i < len; i++){
    var j = i * 2;
    array[i] = parseInt(str.slice(j, j + 2), 16);
  }
  var plain = new Zlib.Gunzip(array).decompress();
  var script = document.createElement('script');
  script.setAttribute('type', 'text/javascript');
  script.setAttribute(
    'src',
    window.URL.createObjectURL(new Blob([plain], {type: 'text/javascript'}));
  );
  document.head.appendChild(script);"""
}

lazy val coreJVM = core.jvm.settings(
  packageDoc in Compile ~= { docJar =>
    IO.withTemporaryDirectory { dir =>
      val files = IO.unzip(docJar, dir)
      val indexJs = dir / "index.js"
      val gz = "index.js.gz"
      val gzFile = dir / gz
      IO.gzip(indexJs, gzFile)
      val binary = IO.readBytes(gzFile).iterator.map("%02x" format _).mkString
      IO.write(indexJs, createJS(gz, binary))
      val out = files.map(f => f -> f.getAbsolutePath.replace(dir.getAbsolutePath + "/", ""))
      println(out)
      IO.zip(out, docJar)
    }
    docJar
  }
)
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
    publishArtifact := false
  )
)

lazy val scalacheckBinding =
  crossProject(JVMPlatform, JSPlatform).crossType(ScalazCrossType)
    .in(file("scalacheck-binding"))
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value,
      osgiExport("scalaz.scalacheck"))
    .dependsOn(core, iteratee)
    .jvmConfigure(_ dependsOn concurrent)
    .jsSettings(scalajsProjectSettings)

lazy val scalacheckBindingJVM = scalacheckBinding.jvm
lazy val scalacheckBindingJS  = scalacheckBinding.js

lazy val tests = crossProject(JSPlatform, JVMPlatform).crossType(ScalazCrossType)
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    publishArtifact := false,
    libraryDependencies += "org.scalacheck" %%% "scalacheck" % scalaCheckVersion.value % "test")
  .dependsOn(core, effect, iteratee, scalacheckBinding)
  .jvmConfigure(_ dependsOn concurrent)
  .jsSettings(scalajsProjectSettings)
  .jsSettings(
    jsEnv := NodeJSEnv().value
  )

lazy val testsJVM = tests.jvm
lazy val testsJS  = tests.js

// can't use "sbt test"
// https://github.com/scala-native/scala-native/issues/339
lazy val nativeTest = Project(nativeTestId, file("nativeTest")).enablePlugins(ScalaNativePlugin)
  .settings(
    standardSettings,
    nativeSettings,
    notPublish
  )
  .dependsOn(iterateeNative)
