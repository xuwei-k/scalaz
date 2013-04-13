package scalaz

////
/**
 * [[https://github.com/ekmett/comonad/blob/v3.0.2/src/Control/Comonad.hs#L186]]
 */
////
trait ComonadApply[F[_]] extends Comonad[F] { self =>
  ////

  // derived functions

  // (<@>) :: w (a -> b) -> w a -> w b
  def <@>[A, B](fa: F[A])(fab: F[A => B]): F[B]


  // (@>) :: w a -> w b -> w b
  // a @> b = const id <$> a <@> b
  def @>[A, B](fa: F[A])(fb: F[B]): F[B] = {
    def f[C, D] = Function.const(identity(_: C))(_: D)
    <@>(fb)(map(fa)(f))
  }


  // (<@) :: w a -> w b -> w a
  // a <@ b = const <$> a <@> b
  def <@[A, B](fa: F[A])(fb: F[B]): F[A] = {
    def f[C] = map(fa)(a => Function.const[A, C](a) _)
    <@>(fb)(f)
  }

  trait ComonadApplyLaws extends ComonadLaws{
    // (.) <$> u <@> v <@> w = u <@> (v <@> w)
    def law1 = ???

    // extract (p <@> q) = extract p (extract q)
    def law2[A, B](p: F[A], q: F[A => F[B]])(implicit FB: Equal[F[B]]): Boolean =
      FB.equal(copoint(q) apply copoint(p), copoint(<@>(p)(q)))

    // duplicate (p <@> q) = (<@>) <$> duplicate p <@> duplicate q
    def law3 = ???

    // (<*>) = (<@>)
    def law4 = ???
  }

  def comonadApplyLaw = new ComonadApplyLaws {}

  ////
  val comonadApplySyntax = new scalaz.syntax.ComonadApplySyntax[F] { def F = ComonadApply.this }
}

object ComonadApply {
  @inline def apply[F[_]](implicit F: ComonadApply[F]): ComonadApply[F] = F

  ////

  ////
}
