package scalaz

////
/**
 *
 */
////
trait ArrowLoop[=>:[_, _]] extends Arrow[=>:] { self =>
  ////

  ////
  val arrowLoopSyntax = new scalaz.syntax.ArrowLoopSyntax[=>:] { def F = ArrowLoop.this }
}

object ArrowLoop {
  @inline def apply[F[_, _]](implicit F: ArrowLoop[F]): ArrowLoop[F] = F

  ////

  ////
}
