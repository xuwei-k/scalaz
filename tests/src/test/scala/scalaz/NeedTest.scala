package scalaz

import std.AllInstances._
import Property.forAll

object NeedTest extends Scalaprops {

  val value = Properties.list(
    laws.bindRec.all[Value],
    laws.monad.all[Value],
    laws.comonad.all[Value],
    laws.traverse1.all[Value],
    laws.zip.all[Value],
    laws.align.all[Value]
  )

  val name = Properties.list(
    laws.bindRec.all[Name],
    laws.monad.all[Name],
    laws.comonad.all[Name],
    laws.traverse1.all[Name],
    laws.zip.all[Name],
    laws.align.all[Name]
  )

  val need = Properties.list(
    laws.bindRec.all[Need],
    laws.monad.all[Need],
    laws.comonad.all[Need],
    laws.traverse1.all[Need],
    laws.zip.all[Need],
    laws.align.all[Need]
  )

  val `clear the Function0 reference` = forAll {
    @volatile var flag = false
    val method = Need.getClass.getMethod("apply", classOf[Function0[_]])
    val need = method.invoke(
      Need,
      new runtime.AbstractFunction0[String]{
        override def finalize = {flag = true}
        override def apply = ""
      }
    ).asInstanceOf[Need[String]]

    flag must_== false
    print(need.value)
    System.gc()
    System.runFinalization()
    flag must_== true
  }

}
