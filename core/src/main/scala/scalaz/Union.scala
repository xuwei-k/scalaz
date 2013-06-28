package scalaz

/**
 * Union types using Curry-Howard isomorphism
 *
 * @see [[http://www.chuusai.com/2011/06/09/scala-union-types-curry-howard/]]
 * @see [[http://en.wikipedia.org/wiki/Curry-Howard_correspondence]]
 */
trait UnionTypes {

  type ![A] = A => Nothing
  type !![A] = ![![A]]

  trait Disj { self =>
    type D
    type t[S] = Disj {
      type D = self.D with ![S]
    }
  }

  type t[T] = {
    type t[S] = (Disj { type D = ![T] })#t[S]
  }

  type or[T <: Disj] = ![T#D]

  type Contains[S, T <: Disj] = !![S] <:< or[T]
  type ∈[S, T <: Disj] = Contains[S, T]

  sealed trait Union[T] {
    val value: Any
  }

  case class Converter[S](s: S) {
    def union[T <: Disj](implicit ev: Contains[S, T]): Union[T] =
      new Union[T] {
        val value = s
      }
  }

  implicit def any2Converter[S](s: S): Converter[S] = Converter[S](s)

}

object UnionTypes extends UnionTypes with UnionInstances

trait UnionInstances {
  import UnionTypes._

  implicit def unionEqual[A: Equal: reflect.ClassTag, B: Equal: reflect.ClassTag]: Equal[Union[t[A]#t[B]]] = 
    Equal.equal{ (x, y) => (x.value, y.value) match{
      case (x: A, y: A) => Equal[A].equal(x, y)
      case (x: B, y: B) => Equal[B].equal(x, y)
      case _            => false
    }}

  implicit def unionInstance[L: reflect.ClassTag] = new Monad[({type λ[α]=Union[t[L]#t[α]]})#λ] {
    def point[A](a: => A) = a.union
    override def map[A, B](fa: Union[t[L]#t[A]])(f: A => B): Union[t[L]#t[B]] = fa.value match{
      case l: L => l.union
      case a: A @unchecked => f(a).union
    }
    def bind[A, B](fa: Union[t[L]#t[A]])(f: A => Union[t[L]#t[B]]): Union[t[L]#t[B]] = fa.value match{
      case l: L => l.union
      case a: A @unchecked => f(a)
    }
  }
}

// vim: expandtab:ts=2:sw=2

