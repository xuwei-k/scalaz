package scalaz

sealed trait NotNothing[A]

object NotNothing {
  implicit def isNotNothing[A]: NotNothing[A] = new NotNothing[A] {}
  implicit def isNothingAmb1: NotNothing[Nothing] = sys.error("ambiguous NotNothing implicit was used")
  implicit def isNothingAmb2: NotNothing[Nothing] = sys.error("ambiguous NotNothing implicit was used")
}

sealed trait NotNothing1 {
  type F[_]
}

object NotNothing1 {
  private[this] val notNothing1 = new NotNothing1 { }

  type NotNothing1Aux[G[_]] = NotNothing1{ type F[A] = G[A] }

  implicit def isNotNothing1[G[_]]: NotNothing1Aux[G] = notNothing1.asInstanceOf[NotNothing1Aux[G]]
  implicit def isNothing1Amb1: NotNothing1Aux[Nothing] = sys.error("ambiguous NotNothing1 implicit was used")
  implicit def isNothing1Amb2: NotNothing1Aux[Nothing] = sys.error("ambiguous NotNothing1 implicit was used")
}
