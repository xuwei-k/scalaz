package scalaz
package syntax

object ApplyBuilder{

  /**
   * @example {{{
   *   import syntax.validation._
   *   val x: ValidationNel[Int, (String, List[Int])] =
   *     %(1.failureNel[String], 2.failureNel[List[Int]]).tupled // Failure(NonEmptyList(1, 2))
   *
   *   import syntax.either._
   *   case class Foo(a: Int, b: List[String])
   *   val y: String \/ Foo = %(1.right[String], List("a").right[String]).run(Foo)
   * }}}
   */
  def %[F1, F2](f1: => F1, f2: => F2)(implicit F1: Unapply[Apply, F1], F2: Unapply[Apply, F2]) =
    new Apply2[F1.M, F2.M, F1.A, F2.A](F1(f1), F2(f2), F1.TC)

  def %%[F1, F2, F3](f1: => F1, f2: => F2, f3: => F3)(implicit F1: Unapply[Apply, F1], F2: Unapply[Apply, F2], F3: Unapply[Apply, F3]) =
    new Apply3[F1.M, F2.M, F3.M, F1.A, F2.A, F3.A](F1(f1), F2(f2), F3(f3), F1.TC)

  def %%%[F1, F2, F3, F4](f1: => F1, f2: => F2, f3: => F3, f4: => F4)(implicit F1: Unapply[Apply, F1], F2: Unapply[Apply, F2], F3: Unapply[Apply, F3], F4: Unapply[Apply, F4]) =
    new Apply4[F1.M, F2.M, F3.M, F4.M, F1.A, F2.A, F3.A, F4.A](F1(f1), F2(f2), F3(f3), F4(f4), F1.TC)

  def %%%%[F1, F2, F3, F4, F5](f1: => F1, f2: => F2, f3: => F3, f4: => F4, f5: => F5)(implicit F1: Unapply[Apply, F1], F2: Unapply[Apply, F2], F3: Unapply[Apply, F3], F4: Unapply[Apply, F4], F5: Unapply[Apply, F5]) =
    new Apply5[F1.M, F2.M, F3.M, F4.M, F5.M, F1.A, F2.A, F3.A, F4.A, F5.A](F1(f1), F2(f2), F3(f3), F4(f4), F5(f5), F1.TC)

}

private[scalaz] final class Apply2[F1[_], F2[_], _1, _2](
  f1: => F1[_1], f2: => F2[_2], F: Apply[F1]
){

  def tupled(implicit
    _2: F2[_2] =:= F1[_2]
  ): F1[(_1, _2)] = run(Tuple2.apply)

  def run[Z](f: (_1, _2) => Z)(implicit
    _2: F2[_2] =:= F1[_2]
  ): F1[Z] = F.apply2(f1, _2(f2))(f)

}

private[scalaz] final class Apply3[F1[_], F2[_], F3[_], _1, _2, _3](
  f1: => F1[_1], f2: => F2[_2], f3: => F3[_3], F: Apply[F1]
){

  def tupled(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3]
  ): F1[(_1, _2, _3)] = run(Tuple3.apply)

  def run[Z](f: (_1, _2, _3) => Z)(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3]
  ): F1[Z] = F.apply3(f1, _2(f2), _3(f3))(f)

}

private[scalaz] final class Apply4[F1[_], F2[_], F3[_], F4[_], _1, _2, _3, _4](
  f1: => F1[_1], f2: => F2[_2], f3: => F3[_3], f4: => F4[_4], F: Apply[F1]
){

  def tupled(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3],
    _4: F4[_4] =:= F1[_4]
  ): F1[(_1, _2, _3, _4)] = run(Tuple4.apply)

  def run[Z](f: (_1, _2, _3, _4) => Z)(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3],
    _4: F4[_4] =:= F1[_4]
  ): F1[Z] = F.apply4(f1, _2(f2), _3(f3), _4(f4))(f)

}

private[scalaz] final class Apply5[F1[_], F2[_], F3[_], F4[_], F5[_], _1, _2, _3, _4, _5](
  f1: => F1[_1], f2: => F2[_2], f3: => F3[_3], f4: => F4[_4], f5: => F5[_5], F: Apply[F1]
){

  def tupled(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3],
    _4: F4[_4] =:= F1[_4],
    _5: F5[_5] =:= F1[_5]
  ): F1[(_1, _2, _3, _4, _5)] = run(Tuple5.apply)

  def run[Z](f: (_1, _2, _3, _4, _5) => Z)(implicit
    _2: F2[_2] =:= F1[_2],
    _3: F3[_3] =:= F1[_3],
    _4: F4[_4] =:= F1[_4],
    _5: F5[_5] =:= F1[_5]
  ): F1[Z] = F.apply5(f1, _2(f2), _3(f3), _4(f4), _5(f5))(f)

}

