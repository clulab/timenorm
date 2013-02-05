package info.bethard.timenorm

import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._

case class DateTime(
    fullDateTime: ZonedDateTime,
    baseUnit: TemporalUnit,
    rangeUnit: TemporalUnit) {

  private val fieldFormats = Map[TemporalField, String](
    YEAR -> "%04d",
    MONTH_OF_YEAR -> "-%02d",
    DAY_OF_MONTH -> "-%02d",
    ALIGNED_WEEK_OF_YEAR -> "-W%02d",
    HOUR_OF_DAY -> "T%02d",
    MINUTE_OF_HOUR -> ":%02d",
    SECOND_OF_MINUTE -> ":%02d")

  private val unitToFields = Map[TemporalUnit, Seq[TemporalField]](
    YEARS -> Seq(YEAR),
    MONTHS -> Seq(YEAR, MONTH_OF_YEAR),
    WEEKS -> Seq(YEAR, ALIGNED_WEEK_OF_YEAR),
    DAYS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH),
    HOURS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY),
    MINUTES -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR),
    SECONDS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE))

  protected def toTimeMLValue(unit: TemporalUnit): String = {
    val parts =
      for (field <- this.unitToFields(unit))
        yield this.fieldFormats(field).format(this.fullDateTime.get(field))
    parts.mkString
  }

  val baseTimeMLValue = this.toTimeMLValue(this.baseUnit)

  val rangeTimeMLValue = this.toTimeMLValue(this.rangeUnit)
}
