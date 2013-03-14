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
      case "DAY_OF_WEEKDAY_WEEKEND" => DAY_OF_WEEKDAY_WEEKEND
      case "WEEKDAY_WEEKEND_OF_WEEK" => WEEKDAY_WEEKEND_OF_WEEK
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

private[timenorm] object EASTER_DAY_OF_YEAR extends TemporalField {
  def getName: String = "EasterDayOfYear"
  def getBaseUnit: TemporalUnit = DAYS
  def getRangeUnit: TemporalUnit = YEARS
  def range: ValueRange = ValueRange.of(0, 1)
  def doGet(temporal: TemporalAccessor): Long = {
    val (_, _, isEaster) = this.doGetEasterMonthDayIsEaster(temporal) 
    if (isEaster) 1 else 0
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = DAY_OF_WEEK.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val (easterMonth, easterDay, isEaster) = this.doGetEasterMonthDayIsEaster(temporal) 
    newValue match {
      case 0 => if (isEaster) DAYS.doPlus(temporal, 1) else temporal
      case 1 => DAY_OF_MONTH.doWith(MONTH_OF_YEAR.doWith(temporal, easterMonth), easterDay)
    }
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???

  private def doGetEasterMonthDayIsEaster(temporal: TemporalAccessor): (Int, Int, Boolean) = {
    val year = YEAR.checkValidIntValue(YEAR.doGet(temporal))
    // from http://aa.usno.navy.mil/faq/docs/easter.php
    val century = year / 100
    val n = year - 19 * ( year / 19 )
    val k = ( century - 17 ) / 25
    var i = century - century / 4 - ( century - k ) / 3 + 19 * n + 15
    i = i - 30 * ( i / 30 )
    i = i - ( i / 28 ) * ( 1 - ( i / 28 ) * ( 29 / ( i + 1 ) )
        * ( ( 21 - n ) / 11 ) )
    var j = year + year / 4 + i + 2 - century + century / 4
    j = j - 7 * ( j / 7 )
    val l = i - j
    val month = 3 + ( l + 40 ) / 44
    val day = l + 28 - 31 * ( month / 4 )
    (month, day, MONTH_OF_YEAR.doGet(temporal) == month && DAY_OF_MONTH.doGet(temporal) == day)
  }
}

private[timenorm] object DAY_OF_WEEKDAY_WEEKEND extends TemporalField {
  def getName: String = "DayOfWeekdayWeekend"
  def getBaseUnit: TemporalUnit = DAYS
  def getRangeUnit: TemporalUnit = WEEKDAYS_WEEKENDS
  def range: ValueRange = ValueRange.of(1, 2, 5)
  def doGet(temporal: TemporalAccessor): Long = {
    val dayOfWeek = DAY_OF_WEEK.doGet(temporal)
    if (dayOfWeek <= WEEKDAY_WEEKEND_OF_WEEK.maxWeekDay) dayOfWeek
    else dayOfWeek - WEEKDAY_WEEKEND_OF_WEEK.maxWeekDay
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = DAY_OF_WEEK.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    WEEKDAY_WEEKEND_OF_WEEK.doGet(temporal) match {
      case 0 => DAY_OF_WEEK.doWith(temporal, newValue)
      case 1 => DAY_OF_WEEK.doWith(temporal, newValue + WEEKDAY_WEEKEND_OF_WEEK.maxWeekDay)
    }
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object WEEKDAY_WEEKEND_OF_WEEK extends TemporalField {
  private[timenorm] final val maxWeekDay = 5L
  def getName: String = "WeekdayWeekendOfWeek"
  def getBaseUnit: TemporalUnit = WEEKDAYS_WEEKENDS
  def getRangeUnit: TemporalUnit = WEEKS
  def range: ValueRange = ValueRange.of(0, 1)
  def doGet(temporal: TemporalAccessor): Long = {
    if (DAY_OF_WEEK.doGet(temporal) <= this.maxWeekDay) 0L else 1L
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = DAY_OF_WEEK.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: Temporal](temporal: R, newValue: Long): R = {
    val dayOfWeek = DAY_OF_WEEK.doGet(temporal)
    val isWeekDay = dayOfWeek <= this.maxWeekDay
    val newDayOfWeek = newValue match {
      case 0 => if (isWeekDay) dayOfWeek else dayOfWeek - this.maxWeekDay
      case 1 => if (!isWeekDay) dayOfWeek else math.min(dayOfWeek + this.maxWeekDay, 7)
    }
    DAY_OF_WEEK.doWith(temporal, newDayOfWeek)
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object WEEKDAYS_WEEKENDS extends TemporalUnit {
  def getName: String = "WeekdaysWeekends"
  def getDuration: Duration = Duration.of(3, DAYS)
  def isDurationEstimated: Boolean = true
  def isSupported(temporal: Temporal): Boolean = {
    WEEKDAY_WEEKEND_OF_WEEK.doIsSupported(temporal) &&
    DAYS.isSupported(temporal) && WEEKS.isSupported(temporal)
  }
  def doPlus[R <: Temporal](dateTime: R, periodToAdd: Long): R = {
    val weekAdjusted = WEEKS.doPlus(dateTime, periodToAdd / 2L)
    val weekdayWeekend = WEEKDAY_WEEKEND_OF_WEEK.doGet(dateTime)
    periodToAdd % 2 match {
      case -1 => weekdayWeekend match {
        case 0 => WEEKDAY_WEEKEND_OF_WEEK.doWith(WEEKS.doPlus(weekAdjusted, -1L), 1L)
        case 1 => WEEKDAY_WEEKEND_OF_WEEK.doWith(weekAdjusted, 0L)
      }
      case 0 => weekAdjusted
      case 1 => weekdayWeekend match {
        case 0 => WEEKDAY_WEEKEND_OF_WEEK.doWith(weekAdjusted, 1L)
        case 1 => WEEKDAY_WEEKEND_OF_WEEK.doWith(WEEKS.doPlus(weekAdjusted, 1L), 0L)
      }
    }
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
