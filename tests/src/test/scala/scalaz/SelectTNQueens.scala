package scalaz

import scala.annotation.tailrec
import scalaz.std.list._

/**
  * [[http://www.cs.bham.ac.uk/~mhe/papers/msfp2010/MSFP2010/haskell/monolitic/NQueens.hs]]
  * [[https://qiita.com/lotz/items/22c60ea59e0a510738de]]
  */
object SelectTNQueens extends SpecLite {

  val n = 8
  type Move = Int

  case class Position(x: Move, y: Move)

  type R = Boolean

  def attacks(p1: Position, p2: Position): Boolean =
    p1.x == p2.x || p1.y == p2.y || math.abs(p1.x - p2.x) == math.abs(p1.y - p2.y)

  @tailrec
  def valid(xs: List[Position]): Boolean = xs match {
    case Nil =>
      true
    case u :: vs =>
      !Foldable[List].any(vs)(v => attacks(u, v)) && valid(vs)
  }

  def p(ms: List[Move]): R = valid(ms.zip(0 until n).map(Position.tupled.apply))

  def epsilons: List[List[Move] => Select[R, Move]] = {
    val epsilon = (h: List[Int]) => find((0 until n).toList diff h)
    List.fill(n)(epsilon)
  }

  def find[A](z: List[A]): Select[Boolean, A] = z match {
    case Nil =>
      sys.error("unexpected")
    case x :: Nil =>
      Select(_ => x)
    case x :: xs =>
      Select(p => {
        if (p(x)) {
          x
        } else {
          find(xs).run(p)
        }
      })
  }

  def bigotimes[X](z: List[List[X] => Select[R, X]]): Select[R, List[X]] = z match {
    case Nil =>
      Applicative[Select[R, ?]].point(Nil)
    case e :: es => for {
      x <- e(Nil)
      xs <- bigotimes(es)
    } yield x :: xs
  }

  def optimalPlay: List[Move] = bigotimes(epsilons).run(p)

  "N-Queens" in {
    println(optimalPlay)
    optimalPlay == List(0, 4, 7, 5, 2, 6, 1, 3)
  }
}
