package info.bethard.timenorm

import org.threeten.bp.ZonedDateTime
import org.threeten.bp.format.DateTimeBuilder
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ValueRange

private object DECADE extends TemporalField {
  def getName: String = "Decade"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(-999, +999)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) / 10
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 10)
  
  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

case class DateTime(
    fullDateTime: ZonedDateTime,
    baseUnit: TemporalUnit,
    rangeUnit: TemporalUnit,
    modifier: String = "EXACT") {

  private val fieldFormats = Map[TemporalField, String](
    DECADE -> "%03d",
    YEAR -> "%04d",
    MONTH_OF_YEAR -> "-%02d",
    DAY_OF_MONTH -> "-%02d",
    ALIGNED_WEEK_OF_YEAR -> "-W%02d",
    HOUR_OF_DAY -> "T%02d",
    MINUTE_OF_HOUR -> ":%02d",
    SECOND_OF_MINUTE -> ":%02d")

  private val unitToFields = Map[TemporalUnit, Seq[TemporalField]](
    DECADES -> Seq(DECADE),
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
