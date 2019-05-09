package scalaz

sealed abstract class CofreeInstancesVersionSpecific0 {

  implicit def cofreeEqual[F[_], A](implicit A0: Equal[A], F0: => Equal[F[Cofree[F, A]]]): Equal[Cofree[F, A]] =
    new CofreeEqual[F, A] {
      override def A = A0
      override lazy val F = F0
    }
}

abstract class CofreeInstancesVersionSpecific extends CofreeInstancesVersionSpecific0 {

  implicit def cofreeOrder[F[_], A](implicit A0: Order[A], F0: => Order[F[Cofree[F, A]]]): Order[Cofree[F, A]] =
    new Order[Cofree[F, A]] with CofreeEqual[F, A] {
      override lazy val F = F0
      override def A = A0
      override def order(x: Cofree[F, A], y: Cofree[F, A]) = {
        Monoid[Ordering].append(A.order(x.head, y.head), F0.order(x.tail, y.tail))
      }
    }
}

private trait CofreeEqual[F[_], A] extends Equal[Cofree[F, A]] {
  protected[this] def A: Equal[A]
  protected[this] def F: Equal[F[Cofree[F, A]]]
  override final def equal(x: Cofree[F, A], y: Cofree[F, A]) = {
    A.equal(x.head, y.head) && F.equal(x.tail, y.tail)
  }
}
