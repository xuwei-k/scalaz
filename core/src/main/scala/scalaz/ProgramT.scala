package scalaz

sealed abstract class PromptT[I[_], M[_], A]

object PromptT {
  final case class Return[I[_], M[_], A] private[PromptT] (a: A) extends PromptT[I, M, A]
  abstract case class :>>=[I[_], M[_], A] private[PromptT] () extends PromptT[I, M, A] {
    type B
    def a: I[B]
    def f: B => ProgramT[I, M, A]
  }

  def return_[I[_], M[_], A](a: A): PromptT[I, M, A] = new Return(a)
  def bind[I[_], M[_], A, B0](a0: I[B0])(f0: B0 => ProgramT[I, M, A]): PromptT[I, M, A] =
    new :>>=[I, M, A]{
      type B = B0
      val a = a0
      val f = f0
    }
}

object Operational{

  def viewT[I[_], M[_], A](a: ProgramT[I, M, A])(implicit M: Monad[M]): M[PromptT[I, M, A]] = {
    import ProgramT._
    val N = Monad[({type λ[α] = ProgramT[I, M, α]})#λ]
    a match {
      case Lift(m)     => M.bind(m)(a => M.point(PromptT.return_[I, M, A](a)))
      case Instr(i)    => M.point(PromptT.bind(i)(N.point(_)))
      case a0 @ Bind() => a0.a match {
        case Lift(m)     => M.bind(m)(a => viewT(a0.f(a)))
        case a1 @ Bind() => viewT(ProgramT.bind(a1.a)(x => ProgramT.bind(a1.f(x))(a0.f)))
        case Instr(i)    => M.point(PromptT.bind(i)(a0.f))
      }
    }
  }

}

sealed abstract class ProgramT[I[_], M[_], A]

object ProgramT {
  final case class Lift[I[_], M[_], A] private[ProgramT] (f: M[A]) extends ProgramT[I, M, A]
  abstract case class Bind[I[_], M[_], A] private[ProgramT]() extends ProgramT[I, M, A] {
    type B
    def a: ProgramT[I, M, B]
    def f: B => ProgramT[I, M, A]
  }
  final case class Instr[I[_], M[_], A] private[ProgramT] (f: I[A]) extends ProgramT[I, M, A]

  def lift[I[_], M[_], A](f: M[A]): ProgramT[I, M, A] =
    new Lift(f)
  def bind[I[_], M[_], A, B0](a0: ProgramT[I, M, B0])(f0: B0 => ProgramT[I, M, A]): ProgramT[I, M, A] =
    new Bind[I, M, A]{
      type B = B0
      def a = a0
      def f = f0
    }
  def instr[I[_], M[_], A](f: I[A]): ProgramT[I, M, A] =
    new Instr(f)

  implicit def programTInstance[I[_], M[_]](implicit M: Monad[M]): Monad[({type λ[α] = ProgramT[I, M, α]})#λ] =
    new Monad[({type λ[α] = ProgramT[I, M, α]})#λ] {
      def point[A](a: => A) = Lift[I, M, A](M.point(a))
      def bind[A, B](fa: ProgramT[I, M, A])(f: A => ProgramT[I, M, B]): ProgramT[I, M, B] =
        ProgramT.bind(fa)(f)
    }
}


