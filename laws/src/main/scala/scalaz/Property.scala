package scalaz

final case class Property(f: (Int, Rand) => (Rand, Result)) {
  def toCheck: Check =
    Check(this)

  def toCheckWith(endo: Endo[Param]): Check =
    Check(this, endo)

  def resize(size: Int): Property =
    Property.fromGen(gen.resize(size))

  def mapSize(g: Int => Int): Property =
    Property.fromGen(gen.mapSize(g))

  def gen: Gen[Result] = Gen.gen(f)

  def and(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res2
        }else if(res2.isProven || res2.isUnfalsified){
          res1
        }else Result.NoResult
      )
    )

  def or(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isFalsified){
          res1
        }else if(res2.isException || res2.isFalsified){
          res2
        }else if(res1.isProven || res1.isUnfalsified){
          res1
        }else if(res2.isProven || res2.isUnfalsified){
          res2
        }else Result.NoResult
      )
    )

  def sequence(p: Property): Property =
    Property.fromGen(
      Apply[Gen].apply2(gen, p.gen)((res1, res2) =>
        if(res1.isException || res1.isProven || res1.isUnfalsified) {
          res1
        }else if(res2.isException || res2.isProven || res2.isUnfalsified){
          res2
        }else if(res1.isFalsified){
          res2
        }else if(res2.isFalsified){
          res1
        }else Result.NoResult
      )
    )

  // TODO remove `listener` parameter?
  def check(param: Param, cancel: () => Boolean, listener: Int => Unit): CheckResult = {
    import param.{rand => _, _}
    @annotation.tailrec
    def loop(s: Int, discarded: Int, sz: Float, random: Rand): CheckResult = if(cancel()) {
      CheckResult.Timeout(s, discarded)
    }else{
      val size = {
        if (s == 0 && discarded == 0) minSize
        else sz + (maxSize - sz) / (minSuccessful - s)
      }

      val r = \/.fromTryCatchThrowable[(Rand, Result), Throwable](
        f(math.round(size), random)
      )

      r match {
        case \/-((nextRand, Result.NoResult)) =>
          if (discarded + 1 >= maxDiscarded) {
            CheckResult.Exhausted(s, discarded + 1)
          } else {
            loop(s, discarded + 1, size, nextRand)
          }
        case \/-((_, Result.Proven)) =>
          CheckResult.Proven(s + 1, discarded)
        case \/-((nextRand, Result.Unfalsified(args))) =>
          if (s + 1 >= minSuccessful) {
            CheckResult.Passed(s + 1, discarded)
          } else {
            listener(s)
            loop(s + 1, discarded, size, nextRand)
          }
        case \/-((_, Result.Falsified(args))) =>
          CheckResult.Falsified(s, discarded, args)
        case \/-((_, Result.Exception(args, ex))) =>
          CheckResult.PropException(s, discarded, args, ex)
        case \/-((_, Result.Ignored(reason))) =>
          CheckResult.Ignored(s, discarded, reason)
        case -\/(e) =>
          CheckResult.GenException(s, discarded, e)
      }
    }

    loop(0, 0, minSize, param.rand)
  }

  def toProperties[A](id: A, param: Endo[Param] = Param.id): Properties[A] =
    Properties.single(id, Check(this, param))

  def ignore(reason: String): Property =
    Property((_, rand) => (rand, Result.Ignored(reason)))
}

object Property {
  private[this] val noResult = propFromResult(Result.NoResult)

  def implies(b: => Boolean, p: => Property): Property =
    if(b) {
      p
    } else {
      noResult
    }


  def fromGen(g: Gen[Result]): Property =
    Property(g.f)

  def propFromResultLazy(r: Need[Result]): Property =
    Property((_, rand) => (rand, r.value))

  def propFromResult(r: Result): Property =
    Property((_, rand) => (rand, r))

  val prop: Boolean => Property = b => propFromResult{
    if(b) Result.Proven
    else Result.Falsified(IList.empty)
  }

  private[this] def propLazy(result: Need[Boolean]): Property =
    propFromResultLazy {
      Functor[Need].map(result){ r =>
        if (r) Result.Proven
        else Result.Falsified(IList.empty)
      }
    }

  def forall0[A](g: Gen[A], shrink: Shrink[A])(f: A => Property): Property =
    Property((i, r) => {
      def first(as: Stream[(Rand, A)], shrinks: Int): Maybe[(A, Result, Rand)] = {
        as.map{ case (rr, a) =>
          val x = exception(f(a), IList(Arg(a, shrinks))).f(i, rr)
          x._2.toMaybe.map(result =>
            (a, result.provenAsUnfalsified.addArg(Arg(a, shrinks)): Result, x._1)
          )
        } match {
          case Stream() =>
            Maybe.empty
          case results @ (h #:: _)=>
            results.find(_.exists(_._2.failed)).getOrElse(h)
        }
      }

      first(Stream(g.f(i, r)), 0) match {
        case Maybe.Just(xx @ (a, re, rand)) if re.failed =>
          @annotation.tailrec
          def loop(shrinks: Int, x: (A, Result, Rand)): (Rand, Result) =
            first(shrink(x._1).map(rand.next -> _), shrinks) match {
              case Maybe.Just((aa, result @ Result.Falsified(args), rr)) if args.count(_.value == aa) <= 1 =>
                loop(shrinks + 1, (aa, result, rr.next))
              case _ =>
                (x._3, x._2)
            }
          loop(1, xx)
        case xx =>
          xx.map(t => (t._3, t._2)).getOrElse((Rand.fromSeed(), Result.NoResult))
      }
    })

  def exception(p: => Property, params: IList[Arg]): Property =
    try {
      p
    } catch {
      case t: Throwable =>
        Property((i, r) => (r, Result.Exception(params, t)))
    }

  def forAll[A](result: => A)(implicit A: AsProperty[A]): Property =
    exception(A.asProperty(result), IList.empty)

  def forAll[A1, Z](f: A1 => Z)(implicit A1: Gen[A1], Z: AsProperty[Z]): Property =
    forAllS(f)(A1, Shrink.empty, Z)

  def forAll[A1, A2, Z](f: (A1, A2) => Z)(implicit A1: Gen[A1], A2: Gen[A2], Z: AsProperty[Z]): Property =
    forAllS(f)(A1, A2, Shrink.empty, Shrink.empty, Z)

  def forAll[A1, A2, A3, Z](f: (A1, A2, A3) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], Z: AsProperty[Z]): Property =
    forAllS(f)(A1, A2, A3, Shrink.empty, Shrink.empty, Shrink.empty, Z)

  def forAll[A1, A2, A3, A4, Z](f: (A1, A2, A3, A4) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], Z: AsProperty[Z]): Property =
    forAllS(f)(A1, A2, A3, A4, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty, Z)

  def forAll[A1, A2, A3, A4, A5, Z](f: (A1, A2, A3, A4, A5) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], Z: AsProperty[Z]): Property =
    forAllS(f)(A1, A2, A3, A4, A5, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty, Shrink.empty, Z)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, Z](A1: Gen[A1])(f: A1 => Z)(implicit Z: AsProperty[Z]): Property =
    forAll[A1, Z](f)(A1, Z)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, Z](A1: Gen[A1], A2: Gen[A2])(f: (A1, A2) => Z)(implicit Z: AsProperty[Z]): Property =
    forAll[A1, A2, Z](f)(A1, A2, Z)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3, Z](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3])(f: (A1, A2, A3) => Z)(implicit Z: AsProperty[Z]): Property =
    forAll[A1, A2, A3, Z](f)(A1, A2, A3, Z)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3, A4, Z](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4])(f: (A1, A2, A3, A4) => Z)(implicit Z: AsProperty[Z]): Property =
    forAll[A1, A2, A3, A4, Z](f)(A1, A2, A3, A4, Z)

  /** `forAll` with explicit `Gen` */
  def forAllG[A1, A2, A3, A4, A5, Z](A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5])(f: (A1, A2, A3, A4, A5) => Z)(implicit Z: AsProperty[Z]): Property =
    forAll[A1, A2, A3, A4, A5, Z](f)(A1, A2, A3, A4, A5, Z)

  def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    forall0(A1, S1)(f)

  def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        f(a1, a2)
      )
    )

  def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          f(a1, a2, a3)
        )
      )
    )

  def property[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1]): Property =
    property1(f)

  def property[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2]): Property =
    property2(f)

  def property[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3]): Property =
    property3(f)

  object NoShrink {
    def property1[A1](f: A1 => Property)(implicit A1: Gen[A1], S1: Shrink[A1] = Shrink.empty[A1]): Property =
      Property.property1(f)

    def property2[A1, A2](f: (A1, A2) => Property)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2]): Property =
      Property.property2(f)

    def property3[A1, A2, A3](f: (A1, A2, A3) => Property)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1] = Shrink.empty[A1], S2: Shrink[A2] = Shrink.empty[A2], S3: Shrink[A3] = Shrink.empty[A3]): Property =
      Property.property3(f)
  }

  def forAllS[A1, Z](f: A1 => Z)(implicit A1: Gen[A1], S1: Shrink[A1], Z: AsProperty[Z]): Property =
    forall0(A1, S1)(f.andThen(Z.toFunction))

  def forAllS[A1, A2, Z](f: (A1, A2) => Z)(implicit A1: Gen[A1], A2: Gen[A2], S1: Shrink[A1], S2: Shrink[A2], Z: AsProperty[Z]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        Z.asProperty(f(a1, a2))
      )
    )

  def forAllS[A1, A2, A3, Z](f: (A1, A2, A3) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3], Z: AsProperty[Z]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          Z.asProperty(f(a1, a2, a3))
        )
      )
    )

  def forAllS[A1, A2, A3, A4, Z](f: (A1, A2, A3, A4) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3], S4: Shrink[A4], Z: AsProperty[Z]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          forall0(A4, S4)(a4 =>
            Z.asProperty(f(a1, a2, a3, a4))
          )
        )
      )
    )

  def forAllS[A1, A2, A3, A4, A5, Z](f: (A1, A2, A3, A4, A5) => Z)(implicit A1: Gen[A1], A2: Gen[A2], A3: Gen[A3], A4: Gen[A4], A5: Gen[A5], S1: Shrink[A1], S2: Shrink[A2], S3: Shrink[A3], S4: Shrink[A4], S5: Shrink[A5], Z: AsProperty[Z]): Property =
    forall0(A1, S1)(a1 =>
      forall0(A2, S2)(a2 =>
        forall0(A3, S3)(a3 =>
          forall0(A4, S4)(a4 =>
            forall0(A5, S5)(a5 =>
              Z.asProperty(f(a1, a2, a3, a4, a5))
            )
          )
        )
      )
    )

}
