package scalaz

////
/**
 *
 */
////
trait Bifoldable1[F[_, _]] extends Bifoldable[F] { self =>
  ////

  def bifoldMap1[A, B, M: Semigroup](fa: F[A, B])(f: A => M)(g: B => M): M

  def bifoldMapRight1[A, B, C](fa: F[A, B])(l: A => C, r: B => C)(f: (A, => C) => C)(g: (B, => C) => C): C

  // derived functions

  def bifoldMapLeft1[A, B, C](fa: F[A, B])(l: A => C, r: B => C)(f: (C, A) => C)(g: (C, B) => C): C =
    bifoldLeft(fa, None: Option[C]){
      case (None, a) => Some(l(a))
      case (Some(c), a) => Some(f(c, a))
    }{
      case (None, b) => Some(r(b))
      case (Some(c), b) => Some(g(c, b))
    }.getOrElse(sys.error("bifoldMapLeft1"))

  override def bifoldMap[A, B, M: Monoid](fa: F[A, B])(f: A => M)(g: B => M): M =
    bifoldMap1(fa)(f)(g)

  override def bifoldRight[A, B, C](fa: F[A, B], z: => C)(f: (A, => C) => C)(g: (B, => C) => C): C =
    bifoldMapRight1(fa)(f(_, z), g(_, z))(f)(g)

  override def bifoldMap1Opt[A, B, M: Semigroup](fa: F[A, B])(f: A => M)(g: B => M): Option[M] =
    Some(bifoldMap1(fa)(f)(g))

  ////
  val bifoldable1Syntax = new scalaz.syntax.Bifoldable1Syntax[F] { def F = Bifoldable1.this }
}

object Bifoldable1 {
  @inline def apply[F[_, _]](implicit F: Bifoldable1[F]): Bifoldable1[F] = F

  ////

  ////
}
