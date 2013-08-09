package scalaz

import collection.immutable.IndexedSeq
import collection.generic.CanBuildFrom

import std.anyVal._
import std.string._
import std.vector._
import syntax.foldable._

/**
 */
final case class Cord(private val self: Vector[String]){

  import Cord.{stringToCord => _, _}

  /**
   * Returns the number of characters in this `Cord`.
   */
  def length: Int = self.foldMap(_.length)

  /**
   * Returns the number of characters in this `Cord`.
   */
  def size: Int = length

  /**
   * Appends another `Cord` to the end of this one.
   */
  def ++(xs: Cord): Cord = cord(self ++ xs.self)

  /**
   * Appends a `String` to the end of this `Cord`.
   */
  def :+(x: String): Cord = cord(self :+ x)

  /**
   * Prepends a `String` to the beginning of this `Cord`.
   */
  def +:(x: String): Cord = cord(x +: self)

  /**
   * Modifies each character in this `Cord` by the given function.
   */
  def map(f: Char => Char): Cord = cord(self map (_ map f))

  def to[F[_]](implicit C: CanBuildFrom[Nothing, Char, F[Char]]): F[Char] =
    self.foldLeft(C()){(buf, vector) =>
      vector.foreach(buf += _)
      buf
    }.result

  def toList: List[Char] = to[List]

  def toStream: Stream[Char] = to[Stream]

  override def toString: String = {
    val sb = new StringBuilder()
    self foreach (sb ++= _)
    sb.toString
  }

  /** Transforms each character to a `Cord` according to the given function and concatenates them all into one `Cord`. */
  def flatMap(f: Char => Cord): Cord =
    cord(self.flatMap(_.flatMap(f(_).self)))
}

object Cord {
  private def cord[A](v: Vector[String]): Cord = new Cord(v)

  implicit def stringToCord(s: String): Cord = cord(Vector(s))

  val empty: Cord = apply()

  def apply(as: Cord*): Cord = as.foldLeft(empty)(_ ++ _)

  def fromStrings[A](as: Iterable[String]): Cord = cord(as.toVector)

  def mkCord(sep: Cord, as: Cord*): Cord =
    if (as.length > 0)
      as.tail.foldLeft(as.head)(_ ++ sep ++ _)
    else
      empty

  implicit val CordInstance: Show[Cord] = new Show[Cord] {
    override def show(x: Cord) = x
    override def shows(x: Cord) = x.toString
  }
  implicit val CordMonoid: Monoid[Cord] = new Monoid[Cord] {
    def zero = empty
    def append(x: Cord, y: => Cord) = x ++ y
  }
  implicit val CordEqual: Equal[Cord] = new Equal[Cord] {
    def equal(x: Cord, y: Cord) = Equal[Vector[String]].equal(x.self, y.self)
  }
}
