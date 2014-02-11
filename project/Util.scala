
object Util {
  def initLower(s: String) = {
    val (init, rest) = s.splitAt(1)
    init.toLowerCase + rest
  }

  def invoke[A](loader: ClassLoader, className: String, method: String, params: (Class[_], AnyRef) *): A = {
    invoke(loader.loadClass(className + "$"), method, params: _*)
  }

  def invoke[A](clazz: Class[_], method: String, params: (Class[_], AnyRef) *): A = {
    clazz.getMethod(method, params.map(_._1): _*).invoke(clazz.getField("MODULE$").get(null), params.map(_._2): _*).asInstanceOf[A]
  }
}
