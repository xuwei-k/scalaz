package scalaz

////
/**
 * A [[scalaz.Category]] supporting all ordinary functions, as well as
 * combining arrows product-wise.  Every Arrow forms a
 * [[scalaz.Contravariant]] in one type parameter, and a
 * [[scalaz.Applicative]] in the other, just as with ordinary
 * functions.
 */
////
trait Arrow[=>:[_, _]] extends Split[=>:] with Strong[=>:] with Category[=>:] { self =>
  ////

  /** Lift an ordinary function. */
  def arr[A, B](f: A => B): A =>: B

  override def covariantInstance[C]: Applicative[C =>: ?] =
    new Applicative[C =>: ?] with SndCovariant[C] {
      def point[A](a: => A): C =>: A = arr(_ => a)
      def ap[A, B](fa: => (C =>: A))(f: => (C =>: (A => B))): (C =>: B) = <<<(arr((y: (A => B, A)) => y._1(y._2)), combine(f, fa))
    }

  /** Alias for `compose`. */
  final def <<<[A, B, C](fbc: (B =>: C), fab: (A =>: B)): =>:[A, C] =
    compose(fbc, fab)

  /** Flipped `<<<`. */
  def >>>[A, B, C](fab: (A =>: B), fbc: (B =>: C)): (A =>: C) =
    compose(fbc, fab)

  /** Pass `C` through untouched. */
  def second[A, B, C](f: (A =>: B)): ((C, A) =>: (C, B)) = {
    def swap[X, Y] = arr[(X, Y), (Y, X)] {
      case (x, y) => (y, x)
    }

    >>>(<<<(first[A, B, C](f), swap), swap)
  }

  /** Alias for `split`. */
  final def splitA[A, B, C, D](fab: (A =>: B), fcd: (C =>: D)): ((A, C) =>: (B, D)) =
    split(fab, fcd)

  /** Run `fab` and `fcd` alongside each other.  Sometimes `***`. */
  def split[A, B, C, D](f: A =>: B, g: C =>: D): ((A,  C) =>: (B, D)) =
    >>>(first[A, B, C](f), second[C, D, B](g))

  /** Run two `fab`s alongside each other. */
  def product[A, B](fab: (A =>: B)): ((A, A) =>: (B, B)) =
    splitA(fab, fab)

  /** Run `fab` and `fac` on the same `A`.  Sometimes `&&&`. */
  def combine[A, B, C](fab: (A =>: B), fac: (A =>: C)): (A =>: (B, C)) =
    >>>(arr((a: A) => (a, a)), splitA(fab, fac))

  /** Contramap on `A`. */
  def mapfst[A, B, C](fab: (A =>: B))(f: C => A): (C =>: B) =
    >>>[C, A, B](arr(f), fab)

  /** Functor map on `B`. */
  def mapsnd[A, B, C](fab: (A =>: B))(f: B => C): (A =>: C) =
    <<<[A, B, C](arr(f), fab)

  trait ArrowLaw extends CategoryLaw {
    import arrowSyntax._
    import std.function._

    def arrowIdentity[A](implicit E: Equal[A =>: A]): Boolean =
      E.equal(arr(identity), id)

    def arrowComposition[A, B, C](ab: A => B, bc: B => C)(implicit E: Equal[A =>: C]): Boolean =
      E.equal(arr(ab andThen bc), arr(ab) >>> arr(bc))

    def arrowExtension[A, B, C](ab: A => B)(implicit E: Equal[(A, C) =>: (B, C)]): Boolean =
      E.equal(arr(ab).first[C], arr(Arrow[Function1].split(ab, identity)))

    def arrowFunctor[A, B, C, D](ab: A =>: B, bc: B =>: C)(implicit E: Equal[(A, D) =>: (C, D)]): Boolean =
      E.equal((ab >>> bc).first[D], ab.first[D] >>> bc.first[D])

    def arrowExchange[A, B, C, D](f: A =>: B, g: C => D)(implicit E: Equal[(A, C) =>: (B, D)]): Boolean =
      E.equal(
        f.first[C] >>> arr(Split[Function1].split(identity[B], g)),
        arr(Split[Function1].split(identity[A], g)) >>> f.first[D]
      )

    def arrowUnit[A, B, C](f: A =>: B)(implicit E: Equal[(A, C) =>: B]): Boolean =
      E.equal(f.first[C] >>> arr(fst[B, C]), arr(fst[A, C]) >>> f)

    def arrowAssociation[A, B, C, D](f: A =>: B)(implicit E: Equal[((A, C), D) =>: (B, (C, D))]): Boolean =
      E.equal(f.first[C].first[D] >>> F.arr(assoc[B, C, D]), F.arr(assoc[A, C, D]) >>> f.first[(C, D)])

    private[this] def fst[A, B](p: (A, B)): A = p._1

    private[this] def assoc[A, B, C](p: ((A, B), C)): (A, (B, C)) = (p._1._1, (p._1._2, p._2))

    // Typeclassopedia
    //
    // 1 arr id  =  id
    // 2 arr (h . g)  =  arr g >>> arr h
    // 3 first (arr g)  =  arr (g *** id)
    // 4 first (g >>> h)  =  first g >>> first h
    // 5 first g >>> arr (id *** h)  =  arr (id *** h) >>> first g
    // 6 first g >>> arr fst  =  arr fst >>> g
    // 7 first (first g) >>> arr assoc  =  arr assoc >>> first g
    // 8 assoc ((x,y),z) = (x,(y,z))
                  
    //first (arr f) = arr (first f)
    //first (f >>> g) = first f >>> first g
    //first f >>> arr fst = arr fst >>> f
    //first f >>> arr (id *** g) = arr (id *** g) >>> first f
    //first (first f) >>> arr assoc = arr assoc >>> first f
  }

  ////
  val arrowSyntax = new scalaz.syntax.ArrowSyntax[=>:] { def F = Arrow.this }
}

object Arrow {
  @inline def apply[F[_, _]](implicit F: Arrow[F]): Arrow[F] = F

  ////

  ////
}
