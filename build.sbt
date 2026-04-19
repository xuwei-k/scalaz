import build._

val minSuccessfulTests = settingKey[Int]("")

lazy val scalaz = Project("scalaz-root", file("."))
  .settings(
    standardSettings,
    description := "scalaz unidoc",
    artifacts := Classpaths.artifactDefs(Seq(Compile / packageDoc, Compile / makePom)).value,
    packagedArtifacts := Classpaths.packaged(Seq(Compile / packageDoc, Compile / makePom)).value,
    pomPostProcess := { node =>
      import scala.xml._
      import scala.xml.transform._
      val rule = new RewriteRule {
        override def transform(n: Node) =
          if (n.label == "dependencies") NodeSeq.Empty else n
      }
      new RuleTransformer(rule).transform(node)(0)
    },
    ScalaUnidoc / unidoc / unidocProjectFilter := {
      //(jsProjects ++ nativeProjects).foldLeft(inAnyProject)((acc, a) => acc -- inProjects(a))
      (ScalaUnidoc / unidoc / unidocProjectFilter).value // TODO
    },
    Defaults.packageTaskSettings(Compile / packageDoc, (Compile / unidoc).map(_.flatMap(Path.allSubpaths)))
  ).enablePlugins(ScalaUnidocPlugin)

lazy val example = projectMatrix
  .in(file("example"))
  .settings(
    standardSettings,
    name := "scalaz-example",
    notPublish,
    Compile / compile / scalacOptions -= "-Xlint:adapted-args",
  )
  .jvmPlatform(
    scalaVersions,
    TaskKey[Unit]("runAllMain") := {
      val r = (run / runner).value
      val classpath = (Compile / fullClasspath).value
      val log = streams.value.log
      (Compile / discoveredMainClasses).value.sorted.foreach(c =>
        r.run(c, classpath.map(_.data), Nil, log)
      )
    },
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      scalaJSUseMainModuleInitializer := true,
      commands += Command.command("runAllMain") { state1 =>
        val extracted = Project.extract(state1)
        val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
        classes.sorted.flatMap(c => s"""set Compile / mainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
      },
    )
  )
  .nativePlatform(
    scalaVersions,
    commands += Command.command("runAllMain") { state1 =>
      val extracted = Project.extract(state1)
      val (state2, classes) = extracted.runTask(Compile / discoveredMainClasses, state1)
      classes.sorted.flatMap(c => s"""set Compile / selectMainClass := Some("$c")""" :: "run" :: Nil).toList ::: state2
    },
  ).dependsOn(
    core, iteratee
  )

lazy val scalacheckBinding =
  projectMatrix
    .in(file("scalacheck-binding"))
    .settings(standardSettings)
    .settings(
      name := "scalaz-scalacheck-binding",
      Compile / compile / scalacOptions -= "-Ywarn-value-discard",
      libraryDependencies += "org.scalacheck" %%% "scalacheck" % "1.19.0",
    )
    .dependsOn(core, iteratee)
    .jvmPlatform(scalaVersions)
    .jsPlatform(
      scalaVersions,
      scalajsProjectSettings
    )
    .nativePlatform(scalaVersions)

lazy val tests = projectMatrix
  .settings(standardSettings)
  .settings(
    name := "scalaz-tests",
    notPublish,
    (Test / testOptions) += {
      val scalacheckOptions = Seq(
        "-maxSize", "5",
        "-workers", "1",
        "-maxDiscardRatio", "50",
        "-minSuccessfulTests", minSuccessfulTests.value.toString
      )
      Tests.Argument(TestFrameworks.ScalaCheck, scalacheckOptions*)
    },
    (Test / sources) := {
      val exclude = Set(
        "LeibnizTest.scala",
        "MonadErrorTest.scala",
        "UnapplyTest.scala",
      )
      val list = (Test / sources).value
      CrossVersion.partialVersion(scalaVersion.value) match {
        case Some((3, _)) =>
          list.filterNot { src =>
            exclude.contains(src.getName)
          }
        case _ =>
          list
      }
    },
  )
  .jvmPlatform(
    scalaVersions,
    minSuccessfulTests := 33,
  )
  .nativePlatform(
    scalaVersions,
    minSuccessfulTests := 33,
  )
  .jsPlatform(
    scalaVersions,
    Def.settings(
      scalajsProjectSettings,
      minSuccessfulTests := 10,
      libraryDependencies += ("org.scala-js" %%% "scalajs-weakreferences" % "1.0.0" % Test).cross(CrossVersion.for3Use2_13)
    )
  )
  .dependsOn(core, effect, iteratee, scalacheckBinding)
