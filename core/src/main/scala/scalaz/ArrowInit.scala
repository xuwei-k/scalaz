package scalaz

////
/**
 *
 */
////
trait ArrowInit[=>:[_, _]] extends ArrowLoop[=>:] { self =>
  ////

  def init[A]: (A =>: A)

  def observe[A, B](ccnf: CCNF[A, B]): A =>: B


  ////
  val arrowInitSyntax = new scalaz.syntax.ArrowInitSyntax[=>:] { def F = ArrowInit.this }
}

object ArrowInit {
  @inline def apply[F[_, _]](implicit F: ArrowInit[F]): ArrowInit[F] = F

  ////

  sealed abstract class CCNF[A, B] extends Product with Serializable
  final case class Arr[A, B](f: A => B) extends CCNF[A, B]
  final case class LoopD[A, B, C](c: C, f: (A, C) => (B, C)) extends CCNF[A, B]

  object CCNF{
    
  }

  ////
}
