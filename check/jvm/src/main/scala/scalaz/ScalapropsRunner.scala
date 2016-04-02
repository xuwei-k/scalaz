package scalaz

import sbt.testing._
import scala.collection.mutable.ArrayBuffer
import scalaz._
import scala.reflect.NameTransformer

object ScalapropsRunner {

  def testFieldNames(clazz: Class[_]): Array[String] =
    Scalaprops.testFieldNames(clazz)

  private[this] def invokeProperty(clazz: Class[_], obj: Scalaprops): List[(String, Property)] =
    Scalaprops.findTestFields(clazz, classOf[Property]).map{ method =>
      val p = method.invoke(obj).asInstanceOf[Property]
      NameTransformer.decode(method.getName) -> p
    }.toList

  private[this] def invokeProperties(clazz: Class[_], obj: Scalaprops): List[Properties[Any]] =
    Scalaprops.findTestFields(clazz, classOf[Properties[_]]).map{ method =>
      val methodName = NameTransformer.decode(method.getName)
      val props = method.invoke(obj).asInstanceOf[Properties[Any]].props
      Properties.noSort[Any](
        Tree.Node(
          methodName -> Maybe.empty,
          props #:: Stream.empty
        )
      )
    }(collection.breakOut)

  private[scalaz] def allProps(clazz: Class[_], obj: Scalaprops, only: Option[NonEmptyList[String]], logger: Logger): Properties[_] = {
    val tests0 = invokeProperty(clazz, obj).map {
      case (name, p) => p.toProperties[Any](name)
    } ::: invokeProperties(clazz, obj)

    val tests = only match {
      case Some(names) =>
        val set = Foldable[NonEmptyList].toSet(names)
        val actualTests: Set[String] = tests0.map(_.id.toString)(collection.breakOut)
        set.filterNot(actualTests).foreach{ typo =>
          logger.warn(s"""'${clazz.getCanonicalName.dropRight(1)}.$typo' does not exists""")
        }
        tests0.filter(p => set(p.id.toString))
      case None =>
        tests0
    }

    Properties.noSort[Any](
      Tree.Node(
        clazz.getName -> Maybe.empty,
        obj.transformProperties(tests).map(_.props)(collection.breakOut)
      )
    )
  }

  private[scalaz] def getTestObject(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader
  ): Scalaprops = {
    val clazz = testClassLoader.loadClass(testClassName + "$")
    clazz.getDeclaredField("MODULE$").get(null).asInstanceOf[Scalaprops]
  }

  private[scalaz] def findTests(
    fingerprint: Fingerprint,
    testClassName: String,
    testClassLoader: ClassLoader,
    only: Option[NonEmptyList[String]],
    logger: Logger
  ): Properties[_] = {
    val clazz = testClassLoader.loadClass(testClassName + "$")
    val obj = getTestObject(fingerprint, testClassName, testClassLoader)
    allProps(clazz, obj, only, logger)
  }

}

final class ScalapropsRunner(
  override val args: Array[String],
  override val remoteArgs: Array[String],
  testClassLoader: ClassLoader
) extends Runner {

  private[this] val status = TestStatus()

  private[this] val results = ArrayBuffer.empty[TestResult]
  private[this] val arguments = Arguments.parse(args.toList)

  override def tasks(taskDefs: Array[TaskDef]) = taskDefs.map{
    taskDef => new ScalapropsTaskImpl(
      taskDef = taskDef,
      testClassLoader = testClassLoader,
      args = args,
      arguments = arguments,
      results = results,
      status = status
    )
  }

  override def done() = {
    s"""done
Total test count: ${status.all}
Failed ${status.failure}, Errors ${status.error}, Passed ${status.success}, Ignored ${status.ignored}
""" + TestResult.formatResults(results, arguments.showDuration)
  }

}
