package scalaz

import java.nio.file.{Files, Paths}
import scala.jdk.StreamConverters.*

object OuterReferenceTest extends SpecLite {

  "test" in {
    val classpath =
     this.getClass.getClassLoader.asInstanceOf[java.net.URLClassLoader].getURLs.map(x => Paths.get(x.toURI).toFile.getCanonicalPath).toList
      .filter(_.endsWith("/classes"))
    assert(classpath.nonEmpty)
    val outerReferences = classpath.flatMap {
      path =>
        val suffix = ".class"
        val dir = Paths.get(path)
        val classFiles = Files.walk(dir)
          .filter(_.toFile.isFile)
          .filter { x =>
            val fileName = x.toFile.getName

            fileName.endsWith(suffix) &&
              fileName.contains('$') &&
              !fileName.contains("$anon$")
          }
          .toScala(List)

        classFiles.map { classFile =>
          dir.relativize(classFile).toString.replace('/', '.').dropRight(suffix.length)
        }.flatMap{ className =>
          val clazz = Class.forName(
            className,
            false,
            this.getClass.getClassLoader
          )
          val outer = clazz.getDeclaredFields.map(_.getName).filter(_ == "$outer")
          Option.when(outer.nonEmpty) {
            className
          }
        }
    }.sorted
    outerReferences.foreach(System.out.println)
    System.out.println(outerReferences.size)
    outerReferences.isEmpty
  }
}