package scalaz

import Isomorphism._
import Free._

/** [[http://stackoverflow.com/questions/16171618/scalaz-trampoline-and-io]] */
object IO_and_Trampoline extends App {

  sealed trait MyOperation[A]
  final case class FromOne[A](value: A) extends MyOperation[A]

  type MyIO[A] = FreeC[MyOperation, A]

  def freeC[T[_], A](ta: T[A]): FreeC[T, A] =
    Free.liftF[({type λ[α]=Coyoneda[T, α]})#λ, A](Coyoneda(ta))
  

  val fromOneIO: () => MyIO[Int] = {
    var i = 0; () => { i += 1; freeC(FromOne(i)) }
  }

  implicit val myOperationCMonad =
    Free.freeMonad[({type λ[α]=Coyoneda[MyOperation, α]})#λ]

  def interpret[M[_], Instr[_], A](f: Instr ~> M, p: FreeC[Instr, A])(implicit M: Monad[M]): M[A] =
    p.resume match {
      case \/-(a) => M.pure(a)
      case -\/(a) => M.bind(f(a.fi))(x => interpret(f, a.k(x)))
    }

  def recMyIOTrampoline(i: Int): TrampolineT[MyIO, Int] =
    if (i == 0)
      TrampolineT.done(fromOneIO())
    else
      TrampolineT.trampolineTMonad[MyIO].apply2(
        recMyIOTrampoline(i - 1),
        TrampolineT.done(fromOneIO())
      )(_ + _)

  val nt = new (MyIO ~> Id.Id){
    def apply[A](a: MyIO[A]) = interpret(
      new (MyOperation ~> Id.Id){
        def apply[A](x: MyOperation[A]) = x match {
          case FromOne(a) => a
        }
      }
    ,a)
  }

  println(
    Hoist[TrampolineT].hoist(nt).apply(
      recMyIOTrampoline(100000)
    ).go
  )

}
