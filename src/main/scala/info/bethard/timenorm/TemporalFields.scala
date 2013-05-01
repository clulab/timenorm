package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.{ Temporal => JTemporal }
import org.threeten.bp.Duration
import org.threeten.bp.Year
import org.threeten.bp.LocalDate
import org.threeten.bp.MonthDay
import org.threeten.bp.LocalTime
import org.threeten.bp.temporal.WeekFields


private[timenorm] abstract class PartialRange(name: String, val field: TemporalField)
extends TemporalUnit {
  def first(temporal: TemporalAccessor): Long
  def last(temporal: TemporalAccessor): Long
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange
  def addToSize(dateTime: JTemporal, periodToAdd: Long): Long
  def range: ValueRange
  def getDuration: Duration
  def isDurationEstimated: Boolean
  
  def getName: String = this.name
  override def toString: String = this.name
  def isSupportedBy(temporal: JTemporal): Boolean = this.field.isSupportedBy(temporal)
  def addTo[R <: JTemporal](dateTime: R, periodToAdd: Long): R = {
    val size = this.addToSize(dateTime, periodToAdd)
    this.field.getBaseUnit().addTo(dateTime, periodToAdd * size)
  }

  def between(dateTime1: JTemporal, dateTime2: JTemporal): Long = ???

  protected def size(first: Long, last: Long, rangeMinimum: Long, rangeMaximum: Long): Long = {
    if (first < last) {
      last - first + 1L
    } else {
      val firstToMax = rangeMaximum - first + 1L
      val minToLast = last - rangeMinimum + 1L
      firstToMax + minToLast
    }
  }
}

private[timenorm] abstract class ConstantPartialRange(
    name: String,
    field: TemporalField,
    first: Long,
    last: Long) extends PartialRange(name, field) {
  private val fixedSize = {
    this.size(first, last, this.field.range().getMinimum(), this.field.range().getMaximum())
  }
  def first(temporal: TemporalAccessor): Long = this.first
  def last(temporal: TemporalAccessor): Long = this.last
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def addToSize(dateTime: JTemporal, periodToAdd: Long): Long = this.fixedSize
  private val rangeMin = this.field.range().getMinimum()
  val range: ValueRange = ValueRange.of(this.rangeMin, this.rangeMin + this.fixedSize - 1)
  val getDuration: Duration = Duration.of(this.fixedSize, this.field.getBaseUnit())
  val isDurationEstimated: Boolean = this.field.getBaseUnit().isDurationEstimated()
}

private[timenorm] abstract class MonthDayPartialRange(
    name: String,
    first: MonthDay,
    last: MonthDay) extends PartialRange(name, DAY_OF_YEAR) {
  def first(temporal: TemporalAccessor): Long = {
    this.first.atYear(YEAR.checkValidIntValue(YEAR.getFrom(temporal))).get(DAY_OF_YEAR)
  }
  def last(temporal: TemporalAccessor): Long =  {
    this.last.atYear(YEAR.checkValidIntValue(YEAR.getFrom(temporal))).get(DAY_OF_YEAR)
  }
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def addToSize(dateTime: JTemporal, periodToAdd: Long): Long = {
    val first = this.first(dateTime)
    val last = this.last(dateTime)
    // partial range does not stretch across range boundaries
    if (first < last) {
      val range = this.field.rangeRefinedBy(dateTime)
      this.size(first, last, range.getMinimum(), range.getMaximum())
    }
    // partial range stretches across two ranges; look at the first partial range
    else if (periodToAdd < 0) {
      val prevDateTime = this.field.getRangeUnit().addTo(dateTime, -1L)
      val prevFirst = this.first(prevDateTime) 
      val max = this.field.rangeRefinedBy(prevDateTime).getMaximum()
      val min = this.field.rangeRefinedBy(dateTime).getMinimum()
      this.size(prevFirst, last, min, max)
    }
    // partial range stretches across two ranges; look at the second partial range
    else {
      val nextDateTime = this.field.getRangeUnit().addTo(dateTime, 1L)
      val nextLast = this.last(nextDateTime)
      val max = this.field.rangeRefinedBy(dateTime).getMaximum()
      val min = this.field.rangeRefinedBy(nextDateTime).getMinimum()
      this.size(first, nextLast, min, max)
    }
  }
  private val sizes = for (year <- Set(1999, 2000)) yield {
    this.addToSize(LocalDate.of(year, 1, 1), +1L)
  }
  val range: ValueRange = ValueRange.of(1, this.sizes.min, this.sizes.max)
  val getDuration: Duration = Duration.of(this.sizes.min, DAYS)
  val isDurationEstimated: Boolean = true
}

private[timenorm] class BaseUnitOfPartial(name: String, partialRange: PartialRange)
extends TemporalField {
  def getName: String = this.name
  override def toString: String = this.name
  def getBaseUnit: TemporalUnit = this.partialRange.field.getBaseUnit()
  def getRangeUnit: TemporalUnit = this.partialRange  
  def range: ValueRange = this.partialRange.range
  def getFrom(temporal: TemporalAccessor): Long = {
    val baseValue = this.partialRange.field.getFrom(temporal) 
    val first = this.partialRange.first(temporal)
    if (baseValue >= first) {
      this.partialRange.rangeRefinedBy(temporal).getMinimum() + baseValue - first
    } else {
      val maxValue = this.partialRange.field.rangeRefinedBy(temporal).getMaximum()
      maxValue - first + 1 + baseValue
    }
  }
  def isSupportedBy(temporal: TemporalAccessor): Boolean = HOUR_OF_DAY.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.partialRange.rangeRefinedBy(temporal)
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = {
    val range = this.partialRange.field.rangeRefinedBy(temporal)
    val rangeMin = range.getMinimum()
    val rangeMax = range.getMaximum()
    val first = this.partialRange.first(temporal)
    val value = first + newValue - rangeMin
    val adjustedValue = if (value <= rangeMax) value else value - rangeMax
    this.partialRange.field.adjustInto(temporal, adjustedValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] class PartialOfRangeUnit(name: String, partialRange: PartialRange)
extends TemporalField {
  def getName: String = this.name
  override def toString: String = this.name
  def getBaseUnit: TemporalUnit = this.partialRange
  def getRangeUnit: TemporalUnit = this.partialRange.field.getRangeUnit()
  def range: ValueRange = ValueRange.of(0, 1)
  def getFrom(temporal: TemporalAccessor): Long = {
    if (this.contains(temporal)) 1L else 0L
  }
  def isSupportedBy(temporal: TemporalAccessor): Boolean = HOUR_OF_DAY.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = newValue match {
    case 1 =>
      if (this.contains(temporal)) temporal
      else this.partialRange.field.adjustInto(temporal, this.partialRange.first(temporal))
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
  
  def contains(temporal: TemporalAccessor): Boolean = {
    val first = this.partialRange.first(temporal)
    val last = this.partialRange.last(temporal)
    val value = this.partialRange.field.getFrom(temporal)
    if (first < last) first <= value && value <= last
    else first <= value || value <= last
  }
}

private[timenorm] object MORNINGS extends ConstantPartialRange("Mornings", HOUR_OF_DAY, 0L, 11L)
private[timenorm] object MORNING_OF_DAY extends PartialOfRangeUnit("MorningOfDay", MORNINGS)
private[timenorm] object HOUR_OF_MORNING extends BaseUnitOfPartial("HourOfMorning", MORNINGS)

private[timenorm] object AFTERNOONS extends ConstantPartialRange("Afternoons", HOUR_OF_DAY, 12L, 17L)
private[timenorm] object AFTERNOON_OF_DAY extends PartialOfRangeUnit("AfternoonOfDay", AFTERNOONS)
private[timenorm] object HOUR_OF_AFTERNOON extends BaseUnitOfPartial("HourOfAfternoon", AFTERNOONS)

private[timenorm] object EVENINGS extends ConstantPartialRange("Evenings", HOUR_OF_DAY, 17L, 23L)
private[timenorm] object EVENING_OF_DAY extends PartialOfRangeUnit("EveningOfDay", EVENINGS)
private[timenorm] object HOUR_OF_EVENING extends BaseUnitOfPartial("HourOfEvening", EVENINGS)

private[timenorm] object NIGHTS extends ConstantPartialRange("Nights", HOUR_OF_DAY, 21L, 3L)
private[timenorm] object NIGHT_OF_DAY extends PartialOfRangeUnit("NightOfDay", NIGHTS)
private[timenorm] object HOUR_OF_NIGHT extends BaseUnitOfPartial("HourOfNight", NIGHTS)

private[timenorm] object WEEKENDS extends ConstantPartialRange("Weekends", DAY_OF_WEEK, 6L, 7L)
private[timenorm] object WEEKEND_OF_WEEK extends PartialOfRangeUnit("WeekendOfWeek", WEEKENDS)
private[timenorm] object DAY_OF_WEEKEND extends BaseUnitOfPartial("DayOfWeekend", WEEKENDS)

private[timenorm] object SPRINGS extends MonthDayPartialRange(
    "Springs", MonthDay.of(3, 20), MonthDay.of(6, 20))
private[timenorm] object SPRING_OF_YEAR extends PartialOfRangeUnit("SpringOfYear", SPRINGS)
private[timenorm] object DAY_OF_SPRING extends BaseUnitOfPartial("DayOfSpring", SPRINGS)

private[timenorm] object SUMMERS extends MonthDayPartialRange(
    "Summers", MonthDay.of(6, 21), MonthDay.of(9, 21))
private[timenorm] object SUMMER_OF_YEAR extends PartialOfRangeUnit("SummerOfYear", SUMMERS)
private[timenorm] object DAY_OF_SUMMER extends BaseUnitOfPartial("DayOfSummer", SUMMERS)

private[timenorm] object FALLS extends MonthDayPartialRange(
    "Falls", MonthDay.of(9, 22), MonthDay.of(12, 20))
private[timenorm] object FALL_OF_YEAR extends PartialOfRangeUnit("FallOfYear", FALLS)
private[timenorm] object DAY_OF_FALL extends BaseUnitOfPartial("DayOfFall", FALLS)

private[timenorm] object WINTERS extends MonthDayPartialRange(
    "Winters", MonthDay.of(12, 21), MonthDay.of(3, 19))
private[timenorm] object WINTER_OF_YEAR extends PartialOfRangeUnit("WinterOfYear", WINTERS)
private[timenorm] object DAY_OF_WINTER extends BaseUnitOfPartial("DayOfWinter", WINTERS)


private[timenorm] object EASTER_DAY_OF_YEAR extends TemporalField {
  def getName: String = "EasterDayOfYear"
  def getBaseUnit: TemporalUnit = DAYS
  def getRangeUnit: TemporalUnit = YEARS
  def range: ValueRange = ValueRange.of(0, 1)
  def getFrom(temporal: TemporalAccessor): Long = {
    val (_, _, isEaster) = this.getFromEasterMonthDayIsEaster(temporal) 
    if (isEaster) 1 else 0
  }
  def isSupportedBy(temporal: TemporalAccessor): Boolean = DAY_OF_WEEK.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = {
    val (easterMonth, easterDay, isEaster) = this.getFromEasterMonthDayIsEaster(temporal) 
    newValue match {
      case 0 => if (isEaster) DAYS.addTo(temporal, 1) else temporal
      case 1 => DAY_OF_MONTH.adjustInto(MONTH_OF_YEAR.adjustInto(temporal, easterMonth), easterDay)
    }
  }
  override def toString: String = this.getName

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???

  private def getFromEasterMonthDayIsEaster(temporal: TemporalAccessor): (Int, Int, Boolean) = {
    val year = YEAR.checkValidIntValue(YEAR.getFrom(temporal))
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
    (month, day, MONTH_OF_YEAR.getFrom(temporal) == month && DAY_OF_MONTH.getFrom(temporal) == day)
  }
}

private[timenorm] object ISO_WEEK {
  val OF_YEAR = WeekFields.ISO.weekOfYear()
}

private[timenorm] object DECADE extends TemporalField {
  def getName: String = "Decade"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(-999, +999)
  def getFrom(temporal: TemporalAccessor): Long = YEAR.getFrom(temporal) / 10
  def isSupportedBy(temporal: TemporalAccessor): Boolean = YEAR.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = YEAR.adjustInto(temporal, newValue * 10)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] object YEAR_OF_DECADE extends TemporalField {
  def getName: String = "YearOfDecade"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(0, 9)
  def getFrom(temporal: TemporalAccessor): Long = YEAR.getFrom(temporal) % 10
  def isSupportedBy(temporal: TemporalAccessor): Boolean = YEAR.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.getFrom(temporal)
    YEAR.adjustInto(temporal, oldYear - oldYear % 10L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] object CENTURY extends TemporalField {
  def getName: String = "Century"
  def getBaseUnit: TemporalUnit = CENTURIES
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(-99, +99)
  def getFrom(temporal: TemporalAccessor): Long = YEAR.getFrom(temporal) / 100
  def isSupportedBy(temporal: TemporalAccessor): Boolean = YEAR.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = YEAR.adjustInto(temporal, newValue * 100)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] object DECADE_OF_CENTURY extends TemporalField {
  def getName: String = "DecadeOfCentury"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(0, 9)
  def getFrom(temporal: TemporalAccessor): Long = DECADE.getFrom(temporal) % 10L
  def isSupportedBy(temporal: TemporalAccessor): Boolean = DECADE.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = {
    val oldDecade = DECADE.getFrom(temporal)
    DECADE.adjustInto(temporal, oldDecade - oldDecade % 10L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] object YEAR_OF_CENTURY extends TemporalField {
  def getName: String = "YearOfCentury"
  def getBaseUnit: TemporalUnit = YEARS
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(0, 99)
  def getFrom(temporal: TemporalAccessor): Long = YEAR.getFrom(temporal) % 100
  def isSupportedBy(temporal: TemporalAccessor): Boolean = YEAR.isSupportedBy(temporal)
  def rangeRefinedBy(temporal: TemporalAccessor): ValueRange = this.range
  def adjustInto[R <: JTemporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.getFrom(temporal)
    YEAR.adjustInto(temporal, oldYear - oldYear % 100L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor) = ???
  def resolve(temporal: TemporalAccessor, value: Long) = ???
}

private[timenorm] object UNSPECIFIED extends TemporalUnit {
  def getName: String = "Unspecified"
  override def toString: String = getName
  def getDuration: Duration = FOREVER.getDuration()
  def isDurationEstimated: Boolean = true
  def isSupportedBy(temporal: JTemporal): Boolean = false
  def addTo[R <: JTemporal](dateTime: R, periodToAdd: Long): R = ???
  def between(dateTime1: JTemporal, dateTime2: JTemporal): Long = ???
}
