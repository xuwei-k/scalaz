package scalaz

////
////
trait PlusEmptyParent[F[_]] { self: PlusEmpty[F] =>
  ////

  def unfoldlPsum[S, A](seed: S)(f: S => Maybe[(S, F[A])]): F[A] =
    unfoldlPsumOpt(seed)(f) getOrElse empty

  def unfoldrPsum[S, A](seed: S)(f: S => Maybe[(F[A], S)]): F[A] =
    unfoldrPsumOpt(seed)(f) getOrElse empty

  ////
}
