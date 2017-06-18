package io.github.retronym

import sbt._
import Keys._

object SbtArgsFilePlugin extends AutoPlugin {
  override def trigger = allRequirements
  override def requires = sbt.plugins.JvmPlugin
  val argsFileContents = taskKey[String]("compiler command line")
  val argsFile = taskKey[Unit]("Write compiler command line into an args file suitable for `scalac @target/compile.args`")
  override lazy val projectSettings = List(Compile, Test).flatMap(c => inConfig(c)(Seq(
    argsFileContents := {
      ((scalacOptions.value :+ "-classpath" :+ dependencyClasspath.value.map(_.data.toString).mkString(":")) ++ sources.value.distinct).mkString("\n")
    },
    argsFile := {
      val f = target.value / (c.name + ".args")
      IO.write(f, argsFileContents.value)
      streams.value.log.info("Wrote compiler comand line to: " + f.getAbsolutePath)
    }
  )))
}
