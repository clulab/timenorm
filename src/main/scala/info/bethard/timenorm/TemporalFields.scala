package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.Temporal
import org.threeten.bp.format.DateTimeBuilder
import org.threeten.bp.Duration
import org.threeten.bp.temporal.SimplePeriod
import org.threeten.bp.Year
import org.threeten.bp.LocalDate
import org.threeten.bp.MonthDay

private[timenorm] object TemporalFields {
  def valueOf(name: String): TemporalField = {
    name match {
      case "HOUR_OF_QUARTER" => HOUR_OF_QUARTER
      case "QUARTER_OF_DAY" => QUARTER_OF_DAY
      case "DAY_OF_SEASON" => DAY_OF_SEASON
      case "SEASON_OF_YEAR" => SEASON_OF_YEAR
      case "YEAR_OF_DECADE" => YEAR_OF_DECADE
      case "DECADE" => DECADE
      case "YEAR_OF_CENTURY" => YEAR_OF_CENTURY
      case "CENTURY" => CENTURY
      case _ => ChronoField.valueOf(name)
    }
  }
}

private[timenorm] object HOUR_OF_QUARTER extends TemporalField {
  def getName: String = "HourOfQuarter"
  def getBaseUnit: TemporalUnit = HOURS
  def getRangeUnit: TemporalUnit = QUARTER_DAYS
  def range: ValueRange = ValueRange.of(0, 5)
  def doGet(temporal: TemporalAccessor): Long = HOUR_OF_DAY.doGet(temporal) % 6
  def doIsSupported(temporal: TemporalAccessor): Boolean = HOUR_OF_DAY.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    HOURS.doPlus(temporal, (newValue - this.doGet(temporal)))
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object QUARTER_OF_DAY extends TemporalField {
  def getName: String = "QuarterOfDay"
  def getBaseUnit: TemporalUnit = QUARTER_DAYS
  def getRangeUnit: TemporalUnit = DAYS
  def range: ValueRange = ValueRange.of(0, 3)
  def doGet(temporal: TemporalAccessor): Long = {
    val secondsPerPart = SECOND_OF_DAY.doRange(temporal).getMaximum()  / 4
    SECOND_OF_DAY.doGet(temporal) / secondsPerPart
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = SECOND_OF_DAY.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    QUARTER_DAYS.doPlus(temporal, (newValue - this.doGet(temporal)))
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object QUARTER_DAYS extends TemporalUnit {
  def getName: String = "QuarterDays"
  def getDuration: Duration = Duration.of(6, HOURS)
  def isDurationEstimated: Boolean = true
  def isSupported(temporal: Temporal): Boolean = HOURS.isSupported(temporal)
  def doPlus[R <: Temporal](dateTime: R, periodToAdd: Long): R = {
    HOURS.doPlus(dateTime, periodToAdd * 6)
  }
  override def toString: String = this.getName

  def between[R <: Temporal](dateTime1: R, dateTime2: R): SimplePeriod = ???
}

private[timenorm] object DAY_OF_SEASON extends TemporalField {
  def getName: String = "DayOfSeason"
  def getBaseUnit: TemporalUnit = DAYS
  def getRangeUnit: TemporalUnit = SEASONS
  def range: ValueRange = ValueRange.of(1, 93)
  def doGet(temporal: TemporalAccessor): Long = {
    val year = YEAR.doGet(temporal)
    val (index, begin, end) = this.find(temporal)
    val dayOfSeason = DAY_OF_YEAR.doGet(temporal) - DAY_OF_YEAR.doGet(begin) + 1
    if (dayOfSeason > 0) dayOfSeason
    else dayOfSeason + DAY_OF_YEAR.doRange(begin).getMaximum()
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = {
    YEAR.doIsSupported(temporal) && DAY_OF_YEAR.doIsSupported(temporal)
  }
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val (index, begin, end) = this.find(temporal)
    val atBegin = DAY_OF_YEAR.doWith(YEAR.doWith(temporal, begin.getYear()), begin.get(DAY_OF_YEAR))
    DAYS.doPlus(atBegin, newValue - 1)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???

  private[timenorm] def find(temporal: TemporalAccessor): (Int, LocalDate, LocalDate) = {
    val year = YEAR.checkValidIntValue(YEAR.doGet(temporal))
    val seasonMonthDayRanges = Seq(
      (3, LocalDate.of(year - 1, 12, 21), LocalDate.of(year, 3, 19)),
      (0, LocalDate.of(year, 3, 20), LocalDate.of(year, 6, 20)),
      (1, LocalDate.of(year, 6, 21), LocalDate.of(year, 9, 21)),
      (2, LocalDate.of(year, 9, 22), LocalDate.of(year, 12, 20)),
      (3, LocalDate.of(year, 12, 21), LocalDate.of(year + 1, 3, 19)))
    val dayOfYear = DAY_OF_YEAR.doGet(temporal)
    seasonMonthDayRanges.find {
      case (index, begin, end) => 
        (begin.getYear() < year || DAY_OF_YEAR.doGet(begin) <= dayOfYear) && 
        (year < end.getYear() || dayOfYear <= DAY_OF_YEAR.doGet(end))
    }.get
  }
}

private[timenorm] object SEASON_OF_YEAR extends TemporalField {
  private final val firstDayOfSpring = 80 // March 20 or 21
  
  def getName: String = "SeasonOfYear"
  def getBaseUnit: TemporalUnit = SEASONS
  def getRangeUnit: TemporalUnit = YEARS
  def range: ValueRange = ValueRange.of(0, 3)
  def doGet(temporal: TemporalAccessor): Long = DAY_OF_SEASON.find(temporal)._1
  def doIsSupported(temporal: TemporalAccessor): Boolean = DAY_OF_SEASON.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val oldValue = SEASON_OF_YEAR.doGet(temporal)
    SEASONS.doPlus(temporal, newValue - oldValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object SEASONS extends TemporalUnit {
    def getName: String = "Seasons"
    def getDuration: Duration = Duration.of(91, DAYS)
    def isDurationEstimated: Boolean = true
    def isSupported(temporal: Temporal): Boolean = {
      DAYS.isSupported(temporal) && YEARS.isSupported(temporal)
    }
  def doPlus[R <: Temporal](dateTime: R, periodToAdd: Long): R = {
    val oldDayOfSeason = DAY_OF_SEASON.doGet(dateTime)
    val beginDateTime = DAY_OF_SEASON.doWith(dateTime, 1) 
    val oldYear = YEAR.checkValidIntValue(YEAR.doGet(beginDateTime))
    val oldSeason = SEASON_OF_YEAR.doGet(beginDateTime)
    val newSeason = oldSeason + periodToAdd % 4
    val (yearAdjustment, month, day) = newSeason match {
      case -3 => (-1, 6, 21) // summer
      case -2 => (-1, 9, 22) // fall
      case -1 => (-1, 12, 21) // winter
      case 0 => (0, 3, 20) // spring
      case 1 => (0, 6, 21) // summer
      case 2 => (0, 9, 22) // fall
      case 3 => (0, 12, 21) // winter
      case 4 => (+1, 3, 20) // spring
      case 5 => (+1, 6, 21) // summer
      case 6 => (+1, 9, 22) // fall
    }
    val withYear = YEARS.doPlus(beginDateTime, periodToAdd / 4 + yearAdjustment)
    val withYearMonthDay = MONTH_OF_YEAR.doWith(DAY_OF_MONTH.doWith(withYear, day), month)
    DAYS.doPlus(withYearMonthDay, oldDayOfSeason - 1)
  }
  
    def between[R <: Temporal](dateTime1: R, dateTime2: R): SimplePeriod = ???
    override def toString: String = this.getName
}

private[timenorm] object DECADE extends TemporalField {
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

private[timenorm] object YEAR_OF_DECADE extends TemporalField {
  def getName: String = "YearOfDecade"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(0, 9)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) % 10
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.doGet(temporal)
    YEAR.doWith(temporal, oldYear - oldYear % 10L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object CENTURY extends TemporalField {
  def getName: String = "Century"
  def getBaseUnit: TemporalUnit = CENTURIES
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(-99, +99)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) / 100
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 100)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object YEAR_OF_CENTURY extends TemporalField {
  def getName: String = "YearOfCentury"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(0, 99)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) % 100
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.doGet(temporal)
    YEAR.doWith(temporal, oldYear - oldYear % 100L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}
