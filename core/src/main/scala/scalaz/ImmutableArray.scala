package scalaz

import reflect.ClassManifest
import collection.immutable.IndexedSeq
import collection.mutable.{ArrayBuilder, Builder}
import collection.generic.CanBuildFrom
import collection.IndexedSeqOptimized
import syntax.Ops

/**
 * An immutable wrapper for arrays
 *
 * @tparam A type of the elements of the array
 */
sealed abstract class ImmutableArray[+A]
    extends IndexedSeq[A]
    with IndexedSeqOptimized[A, ImmutableArray[A]] {
  implicit protected[this] def elemManifest: ClassManifest[A]

  override def isEmpty: Boolean = length == 0

  override protected[this] def newBuilder: Builder[A, ImmutableArray[A]] =
    ImmutableArray.newBuilder

  override def stringPrefix = "ImmutableArray"

  def ++[B >: A](other: ImmutableArray[B]): ImmutableArray[A]
}

object ImmutableArray extends ImmutableArrayInstances {
  val emptyBooleanImmutableArray : ImmutableArray[Boolean] = fromArray(new Array(0))
  val emptyByteImmutableArray    : ImmutableArray[Byte]    = fromArray(new Array(0))
  val emptyCharImmutableArray    : ImmutableArray[Char]    = fromArray(new Array(0))
  val emptyDoubleImmutableArray  : ImmutableArray[Double]  = fromArray(new Array(0))
  val emptyFloatImmutableArray   : ImmutableArray[Float]   = fromArray(new Array(0))
  val emptyIntImmutableArray     : ImmutableArray[Int]     = fromArray(new Array(0))
  val emptyLongImmutableArray    : ImmutableArray[Long]    = fromArray(new Array(0))
  val emptyShortImmutableArray   : ImmutableArray[Short]   = fromArray(new Array(0))
  val emptyObjectImmutableArray  : ImmutableArray[Object]  = fromArray(new Array(0))

  def empty[A](implicit A: ClassManifest[A]): ImmutableArray[A] = {
    val x = A match{
      case ClassManifest.Boolean => emptyBooleanImmutableArray
      case ClassManifest.Byte    => emptyByteImmutableArray
      case ClassManifest.Char    => emptyCharImmutableArray
      case ClassManifest.Double  => emptyDoubleImmutableArray
      case ClassManifest.Float   => emptyFloatImmutableArray
      case ClassManifest.Int     => emptyIntImmutableArray
      case ClassManifest.Long    => emptyLongImmutableArray
      case ClassManifest.Short   => emptyShortImmutableArray
      case _                     => emptyObjectImmutableArray
    }
    x.asInstanceOf[ImmutableArray[A]]
  }

  def apply[A: ClassManifest](elems: A*): ImmutableArray[A] =
    fromArray(elems.toArray)

  def iterate[A: ClassManifest](start: A, length: Int)(f: A => A): ImmutableArray[A] =
    if(length <= 0)
      empty[A]
    else
      fromArray(Array.iterate(start, length)(f))

  def make[A](x: AnyRef): ImmutableArray[A] = {
    val y = x match {
      case null              => null
      case x: Array[Byte]    => new ofByte(x)
      case x: Array[Short]   => new ofShort(x)
      case x: Array[Char]    => new ofChar(x)
      case x: Array[Int]     => new ofInt(x)
      case x: Array[Long]    => new ofLong(x)
      case x: Array[Float]   => new ofFloat(x)
      case x: Array[Double]  => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit]    => new ofUnit(x)
      case x: Array[AnyRef]  => new ofRef(x)
      case x: String         => new StringArray(x)
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /**
   * Wrap `x` in an `ImmutableArray`.
   *
   * Provides better type inference than `make[A]`
   */
  def fromArray[A](x: Array[A]): ImmutableArray[A] = {
    val y = x.asInstanceOf[AnyRef] match {
      case null              => null
      case x: Array[Byte]    => new ofByte(x)
      case x: Array[Short]   => new ofShort(x)
      case x: Array[Char]    => new ofChar(x)
      case x: Array[Int]     => new ofInt(x)
      case x: Array[Long]    => new ofLong(x)
      case x: Array[Float]   => new ofFloat(x)
      case x: Array[Double]  => new ofDouble(x)
      case x: Array[Boolean] => new ofBoolean(x)
      case x: Array[Unit]    => new ofUnit(x)
      case x: Array[AnyRef]  => new ofRef(x)
    }
    y.asInstanceOf[ImmutableArray[A]]
  }

  /** Wrap the characters in `str` in an `ImmutableArray` */
  def fromString(str: String): ImmutableArray[Char] = new StringArray(str)

  def newBuilder[A](implicit elemManifest: ClassManifest[A]): Builder[A, ImmutableArray[A]] =
    ArrayBuilder.make[A]()(elemManifest).mapResult(make(_))

  def newStringArrayBuilder: Builder[Char, ImmutableArray[Char]] =
    (new StringBuilder).mapResult(fromString(_))

  implicit def canBuildFrom[T](implicit m: ClassManifest[T]): CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] =
    new CanBuildFrom[ImmutableArray[_], T, ImmutableArray[T]] {
      def apply(from: ImmutableArray[_]): Builder[T, ImmutableArray[T]] = newBuilder(m)

      def apply: Builder[T, ImmutableArray[T]] = newBuilder(m)
    }

  implicit def canBuildFromChar(implicit m: ClassManifest[Char]): CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] =
    new CanBuildFrom[ImmutableArray[_], Char, ImmutableArray[Char]] {
      def apply(from: ImmutableArray[_]): Builder[Char, ImmutableArray[Char]] = newStringArrayBuilder

      def apply: Builder[Char, ImmutableArray[Char]] = newStringArrayBuilder
    }

  sealed abstract class ImmutableArray1[+A] private[ImmutableArray](private[this] val arr: Array[A]) extends ImmutableArray[A] {
    override protected[this] def newBuilder = ImmutableArray.newBuilder[A](elemManifest)

    def apply(idx: Int) = arr(idx)

    override def length = arr.length
    override def toArray[B >: A : ClassManifest] = arr.clone.asInstanceOf[Array[B]]
    override def copyToArray[B >: A](xs: Array[B], start: Int, len: Int) { arr.copyToArray(xs, start, len) }

    override def slice(from: Int, until: Int) = fromArray(arr.slice(from, until))

    // TODO can do O(1) for primitives
    override def ++[B >: A](other: ImmutableArray[B]) = {
      val newArr = elemManifest.newArray(length + other.length)
      this.copyToArray(newArr, 0, length)
      other.copyToArray(newArr.asInstanceOf[Array[B]], length, other.length)
      fromArray(newArr)
    }
  }

  final class ofRef[A <: AnyRef](array: Array[A]) extends ImmutableArray1[A](array.clone) {
    protected[this] lazy val elemManifest = ClassManifest.classType[A](array.getClass.getComponentType)
  }

  final class ofByte(array: Array[Byte]) extends ImmutableArray1[Byte](array.clone) {
    protected[this] def elemManifest = ClassManifest.Byte
  }

  final class ofShort(array: Array[Short]) extends ImmutableArray1[Short](array.clone) {
    protected[this] def elemManifest = ClassManifest.Short
  }

  final class ofChar(array: Array[Char]) extends ImmutableArray1[Char](array.clone) {
    protected[this] def elemManifest = ClassManifest.Char
  }

  final class ofInt(array: Array[Int]) extends ImmutableArray1[Int](array.clone) {
    protected[this] def elemManifest = ClassManifest.Int
  }

  final class ofLong(array: Array[Long]) extends ImmutableArray1[Long](array.clone) {
    protected[this] def elemManifest = ClassManifest.Long
  }

  final class ofFloat(array: Array[Float]) extends ImmutableArray1[Float](array.clone) {
    protected[this] def elemManifest = ClassManifest.Float
  }

  final class ofDouble(array: Array[Double]) extends ImmutableArray1[Double](array.clone) {
    protected[this] def elemManifest = ClassManifest.Double
  }

  final class ofBoolean(array: Array[Boolean]) extends ImmutableArray1[Boolean](array.clone) {
    protected[this] def elemManifest = ClassManifest.Boolean
  }

  final class ofUnit(array: Array[Unit]) extends ImmutableArray1[Unit](array.clone) {
    protected[this] def elemManifest = ClassManifest.Unit
  }

  final class StringArray(val str: String) extends ImmutableArray[Char] {
    protected[this] def elemManifest = ClassManifest.Char

    override protected[this] def newBuilder = (new StringBuilder).mapResult(new StringArray(_))

    override def apply(idx: Int) = str(idx)

    override def length = str.length
    override def toArray[B >: Char : ClassManifest] = str.toArray
    override def copyToArray[B >: Char](xs: Array[B], start: Int, len: Int) { str.copyToArray(xs, start, len) }

    override def slice(from: Int, until: Int) = new StringArray(str.slice(from, until))

    def ++[B >: Char](other: ImmutableArray[B]) =
      other match {
        case other: StringArray => new StringArray(str + other.str)
        case _ => {
          val newArr = new Array[Char](length + other.length)
          this.copyToArray(newArr, 0, length)
          other.copyToArray(newArr.asInstanceOf[Array[B]], length, other.length)
          fromArray(newArr)
        }
      }
  }

  final class ImmutableArrayCharW(val self: ImmutableArray[Char]) extends Ops[ImmutableArray[Char]] {
    def asString = self match {
      case a: StringArray => a.str
      case a => a.mkString
    }
  }

  implicit def wrapRopeChar(array: ImmutableArray[Char]): ImmutableArrayCharW = new ImmutableArrayCharW(array)

  @deprecated("use ImmutableArray", "7.1.0")
  type WrappedImmutableArray[+A] = ImmutableArray[A]

  @deprecated("use ImmutableArray", "7.1.0")
  val WrappedImmutableArray = ImmutableArray
}

sealed abstract class ImmutableArrayInstances {

  implicit def immutableArrayEqual[A: Equal]: Equal[ImmutableArray[A]] =
    new ImmutableArrayEqual[A] {
      def A = implicitly
    }

  implicit def immutableArrayOrder[A: Order]: Order[ImmutableArray[A]] =
    new ImmutableArrayOrder[A] {
      def A = implicitly
    }

  implicit def immutableArrayMonoid[A: ClassManifest]: Monoid[ImmutableArray[A]] =
    new Monoid[ImmutableArray[A]] {
      val zero = ImmutableArray.empty[A]
      def append(a: ImmutableArray[A], b: => ImmutableArray[A]) = a ++ b
    }

  implicit val immutableArrayInstance: Plus[ImmutableArray] with Zip[ImmutableArray] with Foldable[ImmutableArray] =
    new Plus[ImmutableArray] with Zip[ImmutableArray] with Foldable[ImmutableArray] with Foldable.FromFoldr[ImmutableArray] {
      def plus[A](a: ImmutableArray[A], b: => ImmutableArray[A]) =
        a ++ b
      def zip[A, B](a: => ImmutableArray[A], b: => ImmutableArray[B]) =
        a zip b
      def foldRight[A, B](fa: ImmutableArray[A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)((a, b) => f(a, b))
      override def foldLeft[A, B](fa: ImmutableArray[A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)
      override def index[A](fa: ImmutableArray[A], i: Int) =
        fa.lift(i)
      override def length[A](fa: ImmutableArray[A]) =
        fa.length
      override def empty[A](fa: ImmutableArray[A]) =
        fa.isEmpty
      override def foldLeft1Opt[A](fa: ImmutableArray[A])(f: (A, A) => A) =
        fa.reduceLeftOption(f)
    }

}

private[scalaz] trait ImmutableArrayEqual[A] extends Equal[ImmutableArray[A]] {
  def A: Equal[A]

  override def equal(a: ImmutableArray[A], b: ImmutableArray[A]) = (a corresponds b)(A.equal)
}

private[scalaz] trait ImmutableArrayOrder[A] extends Order[ImmutableArray[A]] with ImmutableArrayEqual[A] {
  def A: Order[A]

  def order(a1: ImmutableArray[A], a2: ImmutableArray[A]): Ordering = {
    import scalaz.Ordering._
    val i1 = a1.iterator
    val i2 = a2.iterator

    while (i1.hasNext && i2.hasNext) {
      val a1 = i1.next()
      val a2 = i2.next()

      val o = A.order(a1, a2)
      if (o != EQ) {
        return o
      }
    }
    std.anyVal.booleanInstance.order(i1.hasNext, i2.hasNext)
  }
}
