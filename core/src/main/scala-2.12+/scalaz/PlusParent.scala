package scalaz

////
import scala.annotation.tailrec
import scalaz.Maybe.Just
////
trait PlusParent[F[_]] { self: Plus[F] =>
  ////

  /**
   * Unfold `seed` to the left and sum using [[#plus]].
   * `Plus` instances with right absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldlPsumOpt[S, A](seed: S)(f: S => Maybe[(S, F[A])]): Maybe[F[A]] = {
    @tailrec def go(s: S, acc: F[A]): F[A] = f(s) match {
      case Just((s, fa)) => go(s, plus(fa, acc))
      case _ => acc
    }
    f(seed) map { case (s, a) => go(s, a) }
  }

  /**
   * Unfold `seed` to the right and sum using [[#plus]].
   * `Plus` instances with left absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldrPsumOpt[S, A](seed: S)(f: S => Maybe[(F[A], S)]): Maybe[F[A]] = {
    @tailrec def go(acc: F[A], s: S): F[A] = f(s) match {
      case Just((fa, s)) => go(plus(acc, fa), s)
      case _ => acc
    }
    f(seed) map { case (a, s) => go(a, s) }
  }

  ////
}
