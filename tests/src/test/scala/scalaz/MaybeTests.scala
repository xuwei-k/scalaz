package scalaz

import Tags._
import Property.forAll

object MaybeTest extends Scalaprops {
  import std.anyVal._
  import std.string._
  import syntax.equal._

  import Maybe._

  private[this] implicit val stringGen: Gen[String] =
    Tag.unsubst(Gen[String @@ GenTags.AlphaNum])

  val order = laws.order.all[Maybe[Int]]
  val firstOrder = laws.order.all[FirstMaybe[Int]]
  val lastOrder = laws.order.all[LastMaybe[Int]]
  val minOrder = laws.order.all[MinMaybe[Int]]
  val maxOrder = laws.order.all[MaxMaybe[Int]]

  val monoid = laws.monoid.all[Maybe[Int]]
  val firstMonoid = laws.monoid.all[FirstMaybe[Int]]
  val lastMonoid = laws.monoid.all[LastMaybe[Int]]
  val minMonoid = laws.monoid.all[MinMaybe[Int]]
  val maxMonoid = laws.monoid.all[MaxMaybe[Int]]

  val firstMonad = laws.monad.all[FirstMaybe]
  val lastMonad = laws.monad.all[LastMaybe]
  val minMonad = laws.monad.all[MinMaybe]
  val maxMonad = laws.monad.all[MaxMaybe]

  val testLaws = Properties.list(
    laws.bindRec.all[Maybe],
    laws.monadPlusStrong.all[Maybe],
    laws.traverse.all[Maybe],
    laws.zip.all[Maybe],
    laws.isEmpty.all[Maybe],
    laws.cobind.all[Maybe],
    laws.align.all[Maybe],
    laws.equal.all[Maybe[Int]]
  )

  val `Empty is less than anything else` = forAll { x: Maybe[Int] => Order[Maybe[Int]].greaterThanOrEqual(x, Maybe.empty) }

  val `Empty is ignored in Maybe[A]@@Min` = forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Min(x) |+| Min(empty)) must_=== Min(x)
  }

  val `Empty is ignored in Maybe[A]@@Max` = forAll { x: Maybe[Int] =>
    import syntax.monoid._
    (Max(x) |+| Max(empty)) must_=== Max(x)
  }

  val `Preserved through Option` = forAll { x: Maybe[Int] => std.option.toMaybe(x.toOption) === x }

  val `just toFailure is failure` = forAll { (x: Int, s: String) => just(x).toFailure(s).isFailure }

  val `empty toFailure is success` = forAll { s: String => empty.toFailure(s).isSuccess }

  val `just toSuccess is success` = forAll { (x: Int, s: String) => just(x).toSuccess(s).isSuccess }

  val `empty toSuccess is failure` = forAll { s: String => empty.toSuccess(s).isFailure }

  val `just toLeft is left` = forAll { (x: Int, s: String) => just(x).toLeft(s).isLeft }

  val `empty toLeft is right` = forAll { s: String => empty.toLeft(s).isRight }

  val `just toRight is right` = forAll { (x: Int, s: String) => just(x).toRight(s).isRight }

  val `empty toRight is left` = forAll { s: String => empty.toRight(s).isLeft }

  val `just isJust` = forAll { x: Int => just(x).isJust }

  val `just isn't empty` = forAll { x: Int => !just(x).isEmpty }

  val `empty is empty` = forAll(empty.isEmpty)

  val `empty isn't just` = forAll(!empty.isJust)

  val `just to option is some` = forAll { x: Int => just(x).toOption.isDefined }

  val `empty to option is none` = forAll(empty.toOption.isEmpty)

  val `just orElse is just` = forAll { (x: Int, m: Maybe[Int]) => just(x).orElse(m).isJust }

  val `fromNullable(null) is Empty` = forAll {
    val s: String = null
    Maybe.fromNullable(s).isEmpty
  }

  val `fromNullable(notNull) is just` = forAll { (s: String) => Maybe.fromNullable(s) must_=== just(s) }

  object instances {
    def equal[A: Equal] = Equal[Maybe[A]]
    def order[A: Order] = Order[Maybe[A]]
    def semigroup[A: Semigroup] = Monoid[Maybe[A]]
    def bindRec = BindRec[Maybe]
    def monad = Monad[Maybe]

    def monoidFirst[A] = Monoid[Maybe[A] @@ First]
    def monoidLast[A] = Monoid[Maybe[A] @@ Last]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[Maybe[A]]
  }
}
