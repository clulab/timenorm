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

object TimeSpan {
  def startingAt(start: ZonedDateTime, period: Period, modifier: Modifier): TimeSpan = {
    TimeSpan(start, period.addTo(start), period, modifier)
  }

  def endingAt(end: ZonedDateTime, period: Period, modifier: Modifier): TimeSpan = {
    TimeSpan(period.subtractFrom(end), end, period, modifier)
  }

  def truncate(time: ZonedDateTime, unit: TemporalUnit): ZonedDateTime = {
    this.unitToFieldsToTruncate(unit).foldLeft(time) {
      case (time, field) => {
        val nUnits = time.get(field) - field.range.getMinimum
        time.minus(nUnits, field.getBaseUnit)
      }
    }
  }

  private val fieldFormats = Map[TemporalField, Int => String](
    (CENTURY, "%02d".format(_)),
    (DECADE, "%03d".format(_)),
    (YEAR, "%04d".format(_)),
    (SPRING_OF_YEAR, _ match { case 1 => "-SP" }),
    (SUMMER_OF_YEAR, _ match { case 1 => "-SU" }),
    (FALL_OF_YEAR, _ match { case 1 => "-FA" }),
    (WINTER_OF_YEAR, _ match { case 1 => "-WI" }),
    (MONTH_OF_YEAR, "-%02d".format(_)),
    (DAY_OF_MONTH, "-%02d".format(_)),
    (ALIGNED_WEEK_OF_YEAR, "-W%02d".format(_)),
    (WEEKEND_OF_WEEK, _ match { case 1 => "-WE" }),
    (MORNING_OF_DAY, _ match { case 1 => "TMO" }),
    (AFTERNOON_OF_DAY, _ match { case 1 => "TAF" }),
    (EVENING_OF_DAY, _ match { case 1 => "TEV" }),
    (NIGHT_OF_DAY, _ match { case 1 => "TNI" }),
    (HOUR_OF_DAY, "T%02d".format(_)),
    (MINUTE_OF_HOUR, ":%02d".format(_)),
    (SECOND_OF_MINUTE, ":%02d".format(_)))

  private val unitToFieldsToDisplay = Map[TemporalUnit, Seq[TemporalField]](
    CENTURIES -> Seq(CENTURY),
    DECADES -> Seq(DECADE),
    YEARS -> Seq(YEAR),
    SPRINGS -> Seq(YEAR, SPRING_OF_YEAR),
    SUMMERS -> Seq(YEAR, SUMMER_OF_YEAR),
    FALLS -> Seq(YEAR, FALL_OF_YEAR),
    WINTERS -> Seq(YEAR, WINTER_OF_YEAR),
    MONTHS -> Seq(YEAR, MONTH_OF_YEAR),
    WEEKS -> Seq(YEAR, ALIGNED_WEEK_OF_YEAR),
    WEEKENDS -> Seq(YEAR, ALIGNED_WEEK_OF_YEAR, WEEKEND_OF_WEEK),
    DAYS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH),
    MORNINGS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, MORNING_OF_DAY),
    AFTERNOONS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, AFTERNOON_OF_DAY),
    EVENINGS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, EVENING_OF_DAY),
    NIGHTS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, NIGHT_OF_DAY),
    HOURS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY),
    MINUTES -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR),
    SECONDS -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE))
  
  private val unitToFieldsToTruncate = Map[TemporalUnit, Seq[TemporalField]](
    CENTURIES -> Seq(YEAR_OF_CENTURY, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    DECADES -> Seq(YEAR_OF_DECADE, MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    YEARS -> Seq(MONTH_OF_YEAR, DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    SPRINGS -> Seq(DAY_OF_SPRING, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    SUMMERS -> Seq(DAY_OF_SUMMER, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    FALLS -> Seq(DAY_OF_FALL, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    WINTERS -> Seq(DAY_OF_WINTER, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    MONTHS -> Seq(DAY_OF_MONTH, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    WEEKS -> Seq(DAY_OF_WEEK, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    WEEKENDS -> Seq(DAY_OF_WEEKEND, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    DAYS -> Seq(HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    MORNINGS -> Seq(HOUR_OF_MORNING, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    AFTERNOONS -> Seq(HOUR_OF_AFTERNOON, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    EVENINGS -> Seq(HOUR_OF_EVENING, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    NIGHTS -> Seq(HOUR_OF_NIGHT, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    HOURS -> Seq(MINUTE_OF_HOUR, SECOND_OF_MINUTE),
    MINUTES -> Seq(SECOND_OF_MINUTE),
    SECONDS -> Seq())
}

case class TimeSpan(
    start: ZonedDateTime,
    end: ZonedDateTime,
    period: Period,
    modifier: Modifier) {

  def timeMLValueOption: Option[String] = {
    if (this.start == this.end) {
      Some(this.start.getDateTime.toString)
    } else {
      this.period.unitAmounts.toList match {
        case List((unit, 1)) if TimeSpan.truncate(this.start, unit) == this.start =>
          val parts =
            for (field <- TimeSpan.unitToFieldsToDisplay(unit))
              yield TimeSpan.fieldFormats(field)(this.start.get(field))
          Some(parts.mkString)
        case _ => None
      }
    }
  }
}
