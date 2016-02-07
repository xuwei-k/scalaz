package scalaz

final case class CogenState[A](rand: Rand, gen: Gen[A])
