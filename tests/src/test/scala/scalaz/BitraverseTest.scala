package scalaz

import scalaz.std.AllInstances.{tuple2Instance => _, _}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object BitraverseTest extends SpecLite {

  implicit val LE = Bitraverse[\/].leftTraverse[Int]
  implicit val RE = Bitraverse[\/].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for Either",  traverse.laws[({type λ[α] = α \/ Int})#λ])
  checkAll("Right-biased Bitraverse for Either", traverse.laws[({type λ[α] = Int \/ α})#λ])


  implicit val LT = Bitraverse[Tuple2].leftTraverse[Int]
  implicit val RT = Bitraverse[Tuple2].rightTraverse[Int]

  checkAll("Left-biased Bitraverse for (,)",  traverse.laws[({type λ[α] = (α, Int)})#λ])
  checkAll("Right-biased Bitraverse for (,)", traverse.laws[({type λ[α] = (Int, α)})#λ])


  "left/right bias" in {
    import scalaz.syntax.either._

    Bitraverse[\/].rightTraverse.traverse(42.left[Int])(x => Vector(x + 3)) must_===(Vector(-\/(42)))
    Bitraverse[\/].leftTraverse.traverse(42.left[Int])(x => Vector(x + 3))  must_===(Vector(-\/(45)))

    Bifoldable[\/].leftFoldable.foldMap(42.left[Int])(identity)  must_===(42)
    Bifoldable[\/].rightFoldable.foldMap(42.left[Int])(identity) must_===(0)
  }

  "bitraverseS, bitraversalS, runBitraverseS does not blow stack" in {
    val N = 100000
    val F = new Bitraverse[({type λ[α, β] = List[α \/ β]})#λ]{
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: List[A \/ B])(f: A => G[C], g: B => G[D]) =
        Traverse[List].traverseImpl(fab)(Bitraverse[\/].bitraverseF(f, g))
    }
    val s = List.tabulate(N)(n =>
      if(n % 2 == 0) \/-(State.modify((_: Int) + 1))
      else -\/(State.modify((_: Int) + 3))
    )
    F.bitraverseS(s)(identity)(identity).exec(0) must_=== N * 2
    F.bitraversalS[Int].run(s)(identity)(identity).exec(0) must_=== N * 2
    F.runBitraverseS(s, 0)(identity)(identity)._1 must_=== N * 2
  }

}

// vim: expandtab:ts=2:sw=2
