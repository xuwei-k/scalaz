package scalaz

final case class Bikleisli[F[_], G[_], A, B](run: F[A] => G[B]){

  def map[C](f: B => C)(implicit G: Functor[G]): Bikleisli[F, G, A, C] =
    Bikleisli(run andThen (G.map(_)(f)))

  def contramap[C](f: C => A)(implicit F: Functor[F]): Bikleisli[F, G, C, B] =
    Bikleisli((fc: F[C]) => run(F.map(fc)(f)))
}

object Bikleisli extends BikleisliInstances {
  def bikleisli[F[_], G[_], A, B](f: F[A] => G[B]): Bikleisli[F, G, A, B] =
    new Bikleisli(f)
}

sealed abstract class BikleisliInstances1 {
  implicit def bikleisliContravariant[F[_], G[_], B](implicit F0: Functor[F]): Contravariant[({type λ[α] = Bikleisli[F, G, α, B]})#λ] =
    new BikleisliContravariant[F, G, B]{
      def F = F0
    }
}

sealed abstract class BikleisliInstances0 extends BikleisliInstances1 {
  implicit def bikleisliFunctor[F[_], G[_], A](implicit F0: Functor[G]): Functor[({type λ[α] = Bikleisli[F, G, A, α]})#λ] =
    new BikleisliFunctor[F, G, A]{
      def F = F0
    }

  implicit def bikleisliProfunctor[F[_], G[_]](implicit F0: Functor[F], G0: Functor[G]): Profunctor[({type λ[α, β] = Bikleisli[F, G, α, β]})#λ] =
    new BikleisliProfunctor[F, G]{
      def F = F0
      def G = G0
    }
}

sealed abstract class BikleisliInstances extends BikleisliInstances0 {
  implicit def bikleisliArrow[F[_], G[_]](implicit F0: Comonad[F], G0: Monad[G], GF0: Distributes[G, F]): Arrow[({type λ[α, β] = Bikleisli[F, G, α, β]})#λ] =
    new BikleisliArrow[F, G] {
      def F = F0
      def G = G0
      def GF = GF0
    }
}


private trait BikleisliContravariant[F[_], G[_], B] extends Contravariant[({type λ[α] = Bikleisli[F, G, α, B]})#λ] {
  implicit def F: Functor[F]

  override def contramap[C, D](fa: Bikleisli[F, G, C, B])(f: D => C) = fa contramap f
}

private trait BikleisliFunctor[F[_], G[_], A] extends Functor[({type λ[α] = Bikleisli[F, G, A, α]})#λ] {
  implicit def F: Functor[G]

  override def map[C, D](fa: Bikleisli[F, G, A, C])(f: C => D) = fa map f
}


private trait BikleisliProfunctor[F[_], G[_]] extends Profunctor[({type λ[α, β] = Bikleisli[F, G, α, β]})#λ] {
  implicit def F: Functor[F]
  implicit def G: Functor[G]

  override def mapfst[A, B, C](fab: Bikleisli[F, G, A, B])(f: C => A) = fab contramap f

  override def mapsnd[A, B, C](fab: Bikleisli[F, G, A, B])(f: B => C) = fab map f
}

private trait BikleisliArrow[F[_], G[_]] extends Arrow[({type λ[α, β] = Bikleisli[F, G, α, β]})#λ] with BikleisliProfunctor[F, G]{
  implicit def F: Comonad[F]
  implicit def G: Monad[G]
  def GF: Distributes[G, F]

  def arr[A, B](f: A => B): Bikleisli[F, G, A, B] =
    Bikleisli((fa: F[A]) => G.point(f(F.copoint(fa))))

  def first[A, B, C](f: Bikleisli[F, G, A, B]): Bikleisli[F, G, (A, C), (B, C)] =
    Bikleisli.bikleisli{ fac =>
      val gb = f.run(F.map(fac)(_._1))
      val (_, c) = F.copoint(fac)
      G.map(gb)((_, c))
    }

  def id[A]: Bikleisli[F, G, A, A] =
    arr[A, A](x => x)

  def compose[A, B, C](f: Bikleisli[F, G, B, C], g: Bikleisli[F, G, A, B]): Bikleisli[F, G, A, C] = {

    ???
  }
}

