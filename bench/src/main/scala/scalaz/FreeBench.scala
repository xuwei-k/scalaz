package scalaz

import org.openjdk.jmh.annotations.Benchmark

object FreeBench {

  val seq10      = (0 to 10)
  val seq100     = (0 to 100)
  val seq1000    = (0 to 1000)
  val seq10000   = (0 to 10000)
  val seq100000  = (0 to 100000)
  val seq1000000 = (0 to 1000000)

  private val z = State[Int, Int](i => (i, 0)).liftF
}

class FreeBench {
  import FreeBench._

  def seqToFree(s: Seq[Int]) =
    s.map(ii =>
      State[Int, Int](i => (i, ii)).liftF
    ).foldLeft(z)(
      (s, a) => s.flatMap(i => a.map(ii => i))
    )

  def freeFoldRunNew(seq: Seq[Int]): Int =
    seqToFree(seq).foldRun(0)((a, b) => b(a))._2

  def freeFoldRunOld(seq: Seq[Int]): Int =
    seqToFree(seq).foldRunOld(0)((a, b) => b(a))._2

  @Benchmark def foldRunNew10: Int      = freeFoldRunNew(seq10)
  @Benchmark def foldRunOld10: Int      = freeFoldRunOld(seq10)

  @Benchmark def foldRunNew100: Int     = freeFoldRunNew(seq100)
  @Benchmark def foldRunOld100: Int     = freeFoldRunOld(seq100)

  @Benchmark def foldRunNew1000: Int    = freeFoldRunNew(seq1000)
  @Benchmark def foldRunOld1000: Int    = freeFoldRunOld(seq1000)

  @Benchmark def foldRunNew10000: Int   = freeFoldRunNew(seq10000)
  @Benchmark def foldRunOld10000: Int   = freeFoldRunOld(seq10000)

  @Benchmark def foldRunNew100000: Int  = freeFoldRunNew(seq100000)
  @Benchmark def foldRunOld100000: Int  = freeFoldRunOld(seq100000)

  @Benchmark def foldRunNew1000000: Int = freeFoldRunNew(seq1000000)
  @Benchmark def foldRunOld1000000: Int = freeFoldRunOld(seq1000000)

}
