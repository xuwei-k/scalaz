package scalaz

////
////
trait MonoidParent[F] { self: Monoid[F] =>
  ////

  def unfoldlSum[S](seed: S)(f: S => Maybe[(S, F)]): F =
    unfoldlSumOpt(seed)(f) getOrElse zero

  def unfoldrSum[S](seed: S)(f: S => Maybe[(F, S)]): F =
    unfoldrSumOpt(seed)(f) getOrElse zero

  ////
}
