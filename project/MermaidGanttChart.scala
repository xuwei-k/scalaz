package sbt // package privateなclassを参照したいので

import java.io.ByteArrayOutputStream
import java.nio.charset.StandardCharsets
import sbt.internal.AbstractTaskExecuteProgress
import sbt.internal.ShutdownHooks
import scala.collection.compat.*
import scala.concurrent.duration.*

/**
  * sbt標準のchrome trace形式ではなく、GitHubのmarkdownでそのまま表示できる形式で出力する用
  * [[https://github.com/sbt/sbt/blob/eec3c32cc841365799eaa5460272e8287b975f10/main/src/main/scala/sbt/internal/TaskTraceEvent.scala]]
  * [[https://github.com/sbt/sbt/pull/4576]]
  * [[https://mermaid.js.org/syntax/gantt.html]]
  */
class MermaidGanttChart extends AbstractTaskExecuteProgress with ExecuteProgress[Task] {
  import AbstractTaskExecuteProgress.Timer

  override def initial(): Unit = ()
  override def afterReady(task: Task[?]): Unit = ()
  override def afterCompleted[T](task: Task[T], result: Result[T]): Unit = ()
  override def afterAllCompleted(results: RMap[Task, Result]): Unit = ()
  override def stop(): Unit = ()

  ShutdownHooks.add(() => report())

  private[this] def report() = {
    if (anyTimings) {
      writeAll()
    }
  }

  def taskFilter(name: String, durationMicros: Long): Boolean = {
    // 一定以上時間がかかったものだけを記録
    durationMicros > 3.seconds.toMicros
  }

  /**
    * 時間がかかった順でtask名と秒数を出力
    */
  private def writeSlowTaskList(values: List[(Task[?], Timer)], write: String => Unit): Unit = {
    if (values.nonEmpty) {
      write("")
      write("## 時間がかかっているtaskの一覧")
      write("")
      values
        .takeRight(20)
        .reverseIterator
        .foreach { case (task, value) =>
          write(s"""1. ${simplifyTaskName(task)} """ + "%.1f".format(value.durationMicros / 1000000.0))
        }
      write("")
    }
  }

  /**
    * 表示上の見やすさのために、区別がつく範囲で名前を短くする
    */
  private def simplifyTaskName[A](t: Task[A]): String = {
    taskName(t)
      .replace(" ", "")
      .replace("/executeTests", "/test")
      .replace("compileIncremental", "compile")
  }

  private def writeAll(): Unit = {
    val out = new ByteArrayOutputStream()
    val values =
      currentTimings
        .filter(x => taskFilter(taskName(x._1), x._2.durationMicros))
        .toList
        .sortBy(_._2.durationMicros)
        .takeRight(100) // 多く出力しすぎても見づらいので、時間がかかった順で固定数にする

    val write: String => Unit = { s =>
      out.write(s.getBytes(StandardCharsets.UTF_8))
      out.write('\n')
    }
    writeTraceEvent(values, write)
    writeSlowTaskList(values, write)
    val result = out.toByteArray
    sys.env.get("GITHUB_STEP_SUMMARY").map(file).filter(_.isFile).foreach { summary =>
      // 存在している場合は、GitHub Actionsが用意してるファイルにも直接書きこむ
      IO.append(summary, result)
    }
    // CIのことだけを考えれば上記だけで十分であるが、ローカルの動作確認用に別のファイルにも書き込む
    IO.write(file("target") / "traces" / "trace.md", result)
  }

  private def writeTraceEvent(values: List[(Task[?], Timer)], write: String => Unit): Unit = {
    values.map(_._2.startMicros).minOption match {
      case Some(start) =>
        write("")
        write("```mermaid")
        write("gantt")
        write("  dateFormat x")
        write("  axisFormat %M:%S")
        val grouped = values.groupBy(_._2.threadId)
        val nowMillis = System.currentTimeMillis()

        def normalize(n: Long): Long = (n - start) + nowMillis

        def durationEvent(name: String, t: Timer): String = {
          val start = normalize(t.startMicros / 1000)
          val end = normalize((t.startMicros + t.durationMicros) / 1000)
          s"""  ${name} : ${start} , ${end}"""
        }

        grouped.foreach { case (threadId, values) =>
          write(s"  section ${threadId}")
          values.sortBy(_._2.startMicros).foreach { case (key, value) =>
            val name = simplifyTaskName(key)
            val n = name + " " + "%.1f".format(value.durationMicros / 1000000.0)
            write("  " + durationEvent(n, value))
          }
        }
        write("```\n\n")
      case None =>
        println("時間がかかったtaskが存在しませんでした")
    }
  }
}
