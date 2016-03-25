package scalaz

private[scalaz] final case class TestResult(
  name: String,
  duration: Long,
  maxSize: Int,
  minSuccessful: Int
) {
  def asSimpleString: String = s"$duration $name $maxSize $minSuccessful"
}

private[scalaz] object TestResult {

  def formatResults(results: Seq[TestResult], count: Int): String = {
    results.sortBy(_.duration)(
      implicitly[scala.Ordering[Long]].reverse
    ).iterator.take(count).map(_.asSimpleString).mkString("\n")
  }

}
