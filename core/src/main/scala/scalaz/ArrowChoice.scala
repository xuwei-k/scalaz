package scalaz

////
/**
 *
 */
////
trait ArrowChoice[F[_, _]] extends Arrow[F] with ProChoice[F] { self =>
  ////

  ////
  val arrowChoiceSyntax = new scalaz.syntax.ArrowChoiceSyntax[F] { def F = ArrowChoice.this }
}

object ArrowChoice {
  @inline def apply[F[_, _]](implicit F: ArrowChoice[F]): ArrowChoice[F] = F

  ////

  ////
}
