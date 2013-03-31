package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Biapplicative` */
trait BiapplicativeOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Biapplicative[F]
  ////

  def bipure(a: A, b: B): F[A, B] = F.bipure(a, b)

  final def <<*>>[C, D](fac: F[A, C])(fabcd: F[A => B, C => D]): F[B, D] = F.biap(fac)(fabcd)

  private def constFunc[X, Y]: X => Y => Y = (_: X) => (y: Y) => y

  final def *>>[C, D](fab: F[A, B], fcd: F[C, D]): F[C, D] = F.biapply2(fab, fcd)(constFunc, constFunc)

  final def <<*[C, D](fab: F[A, B], fcd: F[C, D]): F[A, B] = F.biapply2(fcd, fab)(constFunc, constFunc)

  def biliftA2[C, D, E, H](abc: A => B => C, deh: D => E => H, fad: F[A, D], fbe: F[B, E]): F[C, H] = ???
  //  F.bimap(F.biap(fad) _ )(abc, deh)

  ////
}

trait ToBiapplicativeOps0 {
    implicit def ToBiapplicativeOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Biapplicative, FA]) =
      new BiapplicativeOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Biapplicative[F0.M] = F0.TC }
  
}

trait ToBiapplicativeOps extends ToBiapplicativeOps0 with ToBifunctorOps {
  
  implicit def ToBiapplicativeOps[F[_, _],A, B](v: F[A, B])(implicit F0: Biapplicative[F]) =
      new BiapplicativeOps[F,A, B] { def self = v; implicit def F: Biapplicative[F] = F0 }
  

  ////

  ////
}

trait BiapplicativeSyntax[F[_, _]] extends BifunctorSyntax[F] {
  implicit def ToBiapplicativeOps[A, B](v: F[A, B]): BiapplicativeOps[F, A, B] = new BiapplicativeOps[F, A, B] { def self = v; implicit def F: Biapplicative[F] = BiapplicativeSyntax.this.F }

  def F: Biapplicative[F]
  ////

  ////
}
