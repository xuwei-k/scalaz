package scalaz

import sbt.testing._

final case class ScalapropsEvent(
  fullyQualifiedName: String,
  fingerprint: Fingerprint,
  selector: Selector,
  status: Status,
  throwable: OptionalThrowable,
  duration: Long,
  result: Throwable \&/ CheckResult
) extends Event
