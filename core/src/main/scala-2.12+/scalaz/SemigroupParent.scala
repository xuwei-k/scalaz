package scalaz

////
import scala.annotation.tailrec
import Maybe.Just
////
trait SemigroupParent[F] { self: Semigroup[F] =>
  ////

  /**
   * Unfold `seed` to the left and sum using [[#append]].
   * Semigroups with right absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldlSumOpt[S](seed: S)(f: S => Maybe[(S, F)]): Maybe[F] =
    defaultUnfoldlSumOpt(seed)(f)

  @inline private[this] def defaultUnfoldlSumOpt[S](seed: S)(f: S => Maybe[(S, F)]): Maybe[F] = {
    @tailrec def go(s: S, acc: F): F = f(s) match {
      case Just((s, f)) => go(s, append(f, acc))
      case _ => acc
    }
    f(seed) map { case (s, a) => go(s, a) }
  }

  /**
   * Unfold `seed` to the right and sum using [[#append]].
   * Semigroups with left absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldrSumOpt[S](seed: S)(f: S => Maybe[(F, S)]): Maybe[F] =
    defaultUnfoldrSumOpt(seed)(f)

  @inline private[this] def defaultUnfoldrSumOpt[S](seed: S)(f: S => Maybe[(F, S)]): Maybe[F] = {
    @tailrec def go(acc: F, s: S): F = f(s) match {
      case Just((f, s)) => go(append(acc, f), s)
      case _ => acc
    }
    f(seed) map { case (a, s) => go(a, s) }
  }

  ////
}
