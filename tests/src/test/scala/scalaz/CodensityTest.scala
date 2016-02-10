package scalaz

import std.list._
import std.anyVal._
import std.option._

object CodensityTest extends Scalaprops {

  private[this] final class CodensityEqual[B] {
    import FunctionEqual._
    implicit def equal[F[_], A](implicit F: Gen[A => F[B]], E: Equal[F[B]]): Equal[Codensity[F, A]] =
      Equal[(A => F[B]) => F[B]].contramap(f => f.apply[B] _)
  }

  private[this] val E = new CodensityEqual[Int]
  import E._

  val list = laws.monadPlusStrong.all[Codensity[List, ?]].andThenParam(Param.maxSize(10))
  val option = laws.monadPlusStrong.all[Codensity[Option, ?]]

  val monadTrans = laws.monadTrans.all[Codensity].andThenParamPF{
    case ScalazLaw.monadTransLaw2IList => Param.maxSize(5)
  }

  object instances {
    def functor[F[_]: MonadPlus] = Functor[Codensity[F, ?]]
    def apply[F[_]: MonadPlus] = Apply[Codensity[F, ?]]
    def applicative[F[_]: MonadPlus] = Applicative[Codensity[F, ?]]
    def plus[F[_]: MonadPlus] = Plus[Codensity[F, ?]]
    def monad[F[_]: MonadPlus] = Monad[Codensity[F, ?]]
    def monade[F[_]] = Monad[Codensity[F, ?]]
  }
}
