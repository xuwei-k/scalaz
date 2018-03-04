package scalaz

////
////
trait MonadErrorParent[F[_], S] { self: MonadError[F, S] =>
  ////

  def emap[A, B](fa: F[A])(f: A => S \/ B): F[B] =
    bind(fa)(a => f(a).fold(raiseError(_), pure(_)))

  ////
}
