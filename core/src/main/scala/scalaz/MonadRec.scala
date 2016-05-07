package scalaz

////
/**
 *
 */
////
trait MonadRec[F[_]] extends BindRec[F] with Monad[F] { self =>
  ////

  ////
  val monadRecSyntax = new scalaz.syntax.MonadRecSyntax[F] { def F = MonadRec.this }
}

object MonadRec {
  @inline def apply[F[_]](implicit F: MonadRec[F]): MonadRec[F] = F

  ////

  ////
}
