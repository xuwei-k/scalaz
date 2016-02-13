package scalaz

import Property.forAll

object CokleisliTest extends Scalaprops {

  implicit val cokleisliEqual: Equal[Cokleisli[Option, Int, Int]] =
    Equal.equal{ (a, b) =>
      a(None) == b(None) && Iterator.fill(20)(util.Random.nextInt).map(Option(_)).forall(n => a(n) == b(n))
    }

  val testLaws = Properties.list(
    laws.bindRec.all[Cokleisli[Option, Int, ?]],
    laws.monad.all[Cokleisli[Option, Int, ?]]
  )

  val compose = forAll {
    import std.AllInstances._

    val ck = Cokleisli((a: NonEmptyList[Int]) => a.size)
    val ck1 = ck compose ck
    val run: Int = ck1.run(NonEmptyList(0, 0))
    run must_===(2)
  }

  object instances {
    def bindRec[F[_], W] = BindRec[Cokleisli[F, W, ?]]
    def monad[F[_], W] = Monad[Cokleisli[F, W, ?]]
    def compose[F[_]](implicit F: Cobind[F]) = Compose[Cokleisli[F, ?, ?]]
    def profunctor[F[_]: Functor] = Profunctor[Cokleisli[F, ?, ?]]
    def arrow[F[_] : Comonad] = Arrow[Cokleisli[F, ?, ?]]
    def prochoice[F[_] : Comonad] = ProChoice[Cokleisli[F, ?, ?]]

    // checking absence of ambiguity
    def compose[F[_]](implicit F: Comonad[F]) = Compose[Cokleisli[F, ?, ?]]
    def profunctor[F[_]: Comonad] = Profunctor[Cokleisli[F, ?, ?]]
  }
}
