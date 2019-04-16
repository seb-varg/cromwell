package cromwell.api.model

import java.time.format.DateTimeFormatter
import java.time.{OffsetDateTime, ZoneOffset}

object TimeUtil {
  private val Iso8601MicrosecondsFormat = DateTimeFormatter.ofPattern("uuuu-MM-dd'T'HH:mm:ss.SSSXXXXX")

  implicit class EnhancedOffsetDateTime(val offsetDateTime: OffsetDateTime) extends AnyVal {
    /**
      * Discards the timezone information, converting the timezone to UTC.
      */
    def asUtc: OffsetDateTime = Option(offsetDateTime).map(_.atZoneSameInstant(ZoneOffset.UTC).toOffsetDateTime).orNull

    /**
      * Returns the ISO8601 string of the time in UTC, with microseconds.
      */
    def asUtcString: String = Option(offsetDateTime).map(_.asUtc.format(Iso8601MicrosecondsFormat)).orNull
  }
}
