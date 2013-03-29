package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoField
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ValueRange
import org.threeten.bp.temporal.TemporalAccessor
import org.threeten.bp.temporal.{ Temporal => JTemporal }
import org.threeten.bp.format.DateTimeBuilder
import org.threeten.bp.Duration
import org.threeten.bp.temporal.SimplePeriod
import org.threeten.bp.Year
import org.threeten.bp.LocalDate
import org.threeten.bp.MonthDay
import org.threeten.bp.LocalTime
import org.threeten.bp.temporal.WeekFields


private[timenorm] abstract class PartialRange(name: String, val field: TemporalField)
extends TemporalUnit {
  def first(temporal: TemporalAccessor): Long
  def last(temporal: TemporalAccessor): Long
  def doRange(temporal: TemporalAccessor): ValueRange
  def doPlusSize(dateTime: JTemporal, periodToAdd: Long): Long
  def range: ValueRange
  def getDuration: Duration
  def isDurationEstimated: Boolean
  
  def getName: String = this.name
  override def toString: String = this.name
  def isSupported(temporal: JTemporal): Boolean = this.field.doIsSupported(temporal)
  def doPlus[R <: JTemporal](dateTime: R, periodToAdd: Long): R = {
    val size = this.doPlusSize(dateTime, periodToAdd)
    this.field.getBaseUnit().doPlus(dateTime, periodToAdd * size)
  }

  def between[R <: JTemporal](dateTime1: R, dateTime2: R): SimplePeriod = ???

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
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doPlusSize(dateTime: JTemporal, periodToAdd: Long): Long = this.fixedSize
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
    this.first.atYear(YEAR.checkValidIntValue(YEAR.doGet(temporal))).get(DAY_OF_YEAR)
  }
  def last(temporal: TemporalAccessor): Long =  {
    this.last.atYear(YEAR.checkValidIntValue(YEAR.doGet(temporal))).get(DAY_OF_YEAR)
  }
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doPlusSize(dateTime: JTemporal, periodToAdd: Long): Long = {
    val first = this.first(dateTime)
    val last = this.last(dateTime)
    // partial range does not stretch across range boundaries
    if (first < last) {
      val range = this.field.doRange(dateTime)
      this.size(first, last, range.getMinimum(), range.getMaximum())
    }
    // partial range stretches across two ranges; look at the first partial range
    else if (periodToAdd < 0) {
      val prevDateTime = this.field.getRangeUnit().doPlus(dateTime, -1L)
      val prevFirst = this.first(prevDateTime) 
      val max = this.field.doRange(prevDateTime).getMaximum()
      val min = this.field.doRange(dateTime).getMinimum()
      this.size(prevFirst, last, min, max)
    }
    // partial range stretches across two ranges; look at the second partial range
    else {
      val nextDateTime = this.field.getRangeUnit().doPlus(dateTime, 1L)
      val nextLast = this.last(nextDateTime)
      val max = this.field.doRange(dateTime).getMaximum()
      val min = this.field.doRange(nextDateTime).getMinimum()
      this.size(first, nextLast, min, max)
    }
  }
  private val sizes = for (year <- Set(1999, 2000)) yield {
    this.doPlusSize(LocalDate.of(year, 1, 1), +1L)
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
  def doGet(temporal: TemporalAccessor): Long = {
    val baseValue = this.partialRange.field.doGet(temporal) 
    val first = this.partialRange.first(temporal)
    if (baseValue >= first) {
      this.partialRange.doRange(temporal).getMinimum() + baseValue - first
    } else {
      val maxValue = this.partialRange.field.doRange(temporal).getMaximum()
      maxValue - first + 1 + baseValue
    }
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = HOUR_OF_DAY.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.partialRange.doRange(temporal)
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = {
    val range = this.partialRange.field.doRange(temporal)
    val rangeMin = range.getMinimum()
    val rangeMax = range.getMaximum()
    val first = this.partialRange.first(temporal)
    val value = first + newValue - rangeMin
    val adjustedValue = if (value <= rangeMax) value else value - rangeMax
    this.partialRange.field.doWith(temporal, adjustedValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] class PartialOfRangeUnit(name: String, partialRange: PartialRange)
extends TemporalField {
  def getName: String = this.name
  override def toString: String = this.name
  def getBaseUnit: TemporalUnit = this.partialRange
  def getRangeUnit: TemporalUnit = this.partialRange.field.getRangeUnit()
  def range: ValueRange = ValueRange.of(0, 1)
  def doGet(temporal: TemporalAccessor): Long = {
    if (this.contains(temporal)) 1L else 0L
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = HOUR_OF_DAY.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = newValue match {
    case 1 =>
      if (this.contains(temporal)) temporal
      else this.partialRange.field.doWith(temporal, this.partialRange.first(temporal))
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
  
  def contains(temporal: TemporalAccessor): Boolean = {
    val first = this.partialRange.first(temporal)
    val last = this.partialRange.last(temporal)
    val value = this.partialRange.field.doGet(temporal)
    if (first < last) first <= value && value <= last
    else first <= value || value <= last
  }
}

private[timenorm] object MORNINGS extends ConstantPartialRange("Mornings", HOUR_OF_DAY, 0L, 11L)
private[timenorm] object MORNING_OF_DAY extends PartialOfRangeUnit("MorningOfDay", MORNINGS)
private[timenorm] object HOUR_OF_MORNING extends BaseUnitOfPartial("HourOfMorning", MORNINGS)

private[timenorm] object AFTERNOONS extends ConstantPartialRange("AfternoonOfDay", HOUR_OF_DAY, 12L, 17L)
private[timenorm] object AFTERNOON_OF_DAY extends PartialOfRangeUnit("AfternoonOfDay", AFTERNOONS)
private[timenorm] object HOUR_OF_AFTERNOON extends BaseUnitOfPartial("HourOfAfternoon", AFTERNOONS)

private[timenorm] object EVENINGS extends ConstantPartialRange("EveningOfDay", HOUR_OF_DAY, 17L, 23L)
private[timenorm] object EVENING_OF_DAY extends PartialOfRangeUnit("EveningOfDay", EVENINGS)
private[timenorm] object HOUR_OF_EVENING extends BaseUnitOfPartial("HourOfEvening", EVENINGS)

private[timenorm] object NIGHTS extends ConstantPartialRange("NightOfDay", HOUR_OF_DAY, 21L, 3L)
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
  def doGet(temporal: TemporalAccessor): Long = {
    val (_, _, isEaster) = this.doGetEasterMonthDayIsEaster(temporal) 
    if (isEaster) 1 else 0
  }
  def doIsSupported(temporal: TemporalAccessor): Boolean = DAY_OF_WEEK.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = {
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

private[timenorm] object ISO_WEEK {
  val OF_YEAR = WeekFields.ISO.weekOfYear()
}

private[timenorm] object DECADE extends TemporalField {
  def getName: String = "Decade"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = DECADES
  def range: ValueRange = ValueRange.of(-999, +999)
  def doGet(temporal: TemporalAccessor): Long = YEAR.doGet(temporal) / 10
  def doIsSupported(temporal: TemporalAccessor): Boolean = YEAR.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 10)

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
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = {
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
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = YEAR.doWith(temporal, newValue * 100)

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object DECADE_OF_CENTURY extends TemporalField {
  def getName: String = "DecadeOfCentury"
  def getBaseUnit: TemporalUnit = DECADES
  def getRangeUnit: TemporalUnit = CENTURIES
  def range: ValueRange = ValueRange.of(0, 9)
  def doGet(temporal: TemporalAccessor): Long = DECADE.doGet(temporal) % 10L
  def doIsSupported(temporal: TemporalAccessor): Boolean = DECADE.doIsSupported(temporal)
  def doRange(temporal: TemporalAccessor): ValueRange = this.range
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = {
    val oldDecade = DECADE.doGet(temporal)
    DECADE.doWith(temporal, oldDecade - oldDecade % 10L + newValue)
  }

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
  def doWith[R <: JTemporal](temporal: R, newValue: Long): R = {
    val oldYear = YEAR.doGet(temporal)
    YEAR.doWith(temporal, oldYear - oldYear % 100L + newValue)
  }

  def compare(temporal1: TemporalAccessor, temporal2: TemporalAccessor): Int = ???
  def resolve(builder: DateTimeBuilder, value: Long): Boolean = ???
}

private[timenorm] object UNSPECIFIED extends TemporalUnit {
  def getName: String = "Unspecified"
  override def toString: String = getName
  def getDuration: Duration = FOREVER.getDuration()
  def isDurationEstimated: Boolean = true
  def isSupported(temporal: JTemporal): Boolean = false
  def doPlus[R <: JTemporal](dateTime: R, periodToAdd: Long): R = ???
  def between[R <: JTemporal](dateTime1: R, dateTime2: R): SimplePeriod = ???
}
