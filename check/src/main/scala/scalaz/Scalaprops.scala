package scalaz

import java.lang.reflect.Method
import sbt.testing.Logger
import scala.scalajs.js.annotation.JSExportDescendentObjects
import scalaz._

@JSExportDescendentObjects
trait Scalaprops {

  def param: Param = Param.withCurrentTimeSeed()

  def listener: ScalapropsListener =
    ScalapropsListener.default

  def transformProperties[A](properties: List[Properties[A]]): List[Properties[A]] =
    properties.map(Scalaprops.filterUnitEmpty).sortBy(_.id.toString)

  implicit def toAnyOps[A](actual: => A): AnyOps[A] =
    new AnyOps(actual)

}

object Scalaprops {

  def filterUnitEmpty[A](p: Properties[A]): Properties[A] = {
    def loop(tree: Tree[(A, Maybe[Check])]): Tree[(A, Maybe[Check])] =
      tree match {
        case Tree.Node(root, Stream(Tree.Node((Or.L(()), Maybe.Empty()), sub))) =>
          Tree.Node(root, sub.map(loop))
        case Tree.Node((root, Maybe.Empty()), Stream(Tree.Node(((), sub1), sub2))) =>
          Tree.Node(root -> sub1, sub2.map(loop))
        case _ =>
          Tree.Node(tree.rootLabel, tree.subForest.map(loop))
      }
    Properties.noSort(loop(p.props))
  }

  private[scalaz] def testFieldNames(clazz: Class[_]): Array[String] =
    Array(
      findTestFields(clazz, classOf[Property]),
      findTestFields(clazz, classOf[Properties[_]])
    ).flatten.map(_.getName)

  private[scalaz] def findTestFields(clazz: Class[_], fieldType: Class[_]): Array[Method] =
    clazz.getMethods.filter(method =>
      method.getParameterTypes.length == 0 && method.getReturnType == fieldType
    )

  private[scalaz] def logger(loggers: Array[Logger]): Logger = new Logger {
    override def warn(msg: String): Unit =
      loggers.foreach(_.warn(msg))
    override def error(msg: String): Unit =
      loggers.foreach(_.error(msg))
    override def ansiCodesSupported(): Boolean =
      loggers.forall(_.ansiCodesSupported())
    override def debug(msg: String): Unit =
      loggers.foreach(_.debug(msg))
    override def trace(t: Throwable): Unit =
      loggers.foreach(_.trace(t))
    override def info(msg: String): Unit =
      loggers.foreach(_.info(msg))
  }

}
