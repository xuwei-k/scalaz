package scalaz
package laws

import scalaz.Property.forAll
import scalaz.std.anyVal._

object foldable {
  def leftFMConsistent[F[_], A](implicit F: Foldable[F], afa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldableLaw.leftFMConsistent[A] _)

  def rightFMConsistent[F[_], A](implicit F: Foldable[F], afa: Gen[F[A]], ea: Equal[A]) =
    forAll(F.foldableLaw.rightFMConsistent[A] _)

  def laws[F[_]](implicit fa: Gen[F[Int]], F: Foldable[F]) =
    Properties.properties(ScalazLaw.foldable)(
      ScalazLaw.foldableLeftFMConsistent -> leftFMConsistent[F, Int],
      ScalazLaw.foldableRightFMConsistent -> rightFMConsistent[F, Int]
    )

  def anyIsLazy[F[_], A](implicit F: Foldable[F], arb: Gen[F[A]]) = forAll { fa: F[A] =>
    var i = 0
    F.any(fa){ x =>
      i = i + 1
      true
    }
    val expected = if (F.empty(fa)) 0 else 1
    i == expected
  }

  def allIsLazy[F[_], A](implicit F: Foldable[F], arb: Gen[F[A]]) = forAll { fa: F[A] =>
    var i = 0
    F.all(fa){ x =>
      i = i + 1
      false
    }
    val expected = if (F.empty(fa)) 0 else 1
    i == expected
  }

  def anyConsistent[F[_], A](implicit G: Gen[A => Boolean], F: Foldable[F], fa: Gen[F[A]]) =
    forAll { (fa: F[A], f: A => Boolean) =>
      F.any(fa)(f) == F.toList(fa).exists(f)
    }

  def allConsistent[F[_], A](implicit G: Gen[A => Boolean], F: Foldable[F], fa: Gen[F[A]]) =
    forAll { (fa: F[A], f: A => Boolean) =>
      F.all(fa)(f) == F.toList(fa).forall(f)
    }

  def anyAndAllLazy[F[_]](implicit G: Gen[Int => Boolean], fa: Gen[F[Int]], F: Foldable[F]) =
    Properties.properties(ScalazLaw.foldableAnyAndAll)(
      ScalazLaw.foldableConsistentAny -> anyConsistent[F, Int],
      ScalazLaw.foldableConsistentAll -> allConsistent[F, Int],
      ScalazLaw.foldableAnyIsLazy -> anyIsLazy[F, Int],
      ScalazLaw.foldableAllIsLazy -> allIsLazy[F, Int]
    )

  def all[F[_]](implicit fa: Gen[F[Int]], F: Foldable[F]) =
    laws[F]
}
