package info.bethard.timenorm

import org.threeten.bp.ZonedDateTime
import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.TemporalField
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.temporal.ChronoField._
import org.threeten.bp.temporal.ISOFields._
import scala.collection.immutable.ListMap
import org.threeten.bp.temporal.WeekFields
import org.threeten.bp.LocalDateTime
import org.threeten.bp.ZoneId

sealed trait Temporal {
  val timeMLValue: String
}

case class Period(
    unitAmounts: Map[TemporalUnit, Int],
    modifier: Modifier = Modifier.Exact) extends Temporal {

  private val simplifyUnitMap = ListMap[TemporalUnit, Seq[(TemporalUnit, Int)]](
    DECADES -> Seq((YEARS, 10)),
    CENTURIES -> Seq((DECADES, 10), (YEARS, 100)))

  private val unitChars = ListMap[TemporalUnit, String](
    UNSPECIFIED -> "X",
    CENTURIES -> "CE",
    DECADES -> "DE",
    YEARS -> "Y",
    QUARTER_YEARS -> "Q",
    SPRINGS -> "SP",
    SUMMERS -> "SU",
    FALLS -> "FA",
    WINTERS -> "WI",
    MONTHS -> "M",
    WEEKS -> "W",
    WEEKENDS -> "WE",
    MORNINGS -> "MO",
    AFTERNOONS -> "AF",
    EVENINGS -> "EV",
    NIGHTS -> "NI",
    DAYS -> "D",
    HOURS -> "H",
    MINUTES -> "M",
    SECONDS -> "S")

  val timeMLValue: String = {
    val simpleUnitAmounts = this.simplifyUnitMap.foldLeft(this.unitAmounts) {
      case (counts, (unit, unitMultipliers)) => counts.get(unit) match {
        case None => counts
        case Some(Int.MaxValue) => counts
        case Some(value) => unitMultipliers.find(um => counts.contains(um._1)) match {
          case None => counts
          case Some((newUnit, multiplier)) =>
            counts - unit + (newUnit -> (counts(newUnit) + value * multiplier))
        }
      }
    }
    val parts = for (unit <- simpleUnitAmounts.keySet.toSeq.sortBy(_.getDuration).reverse) yield {
      val amount = simpleUnitAmounts(unit) match {
        case Int.MaxValue => "X"
        case i => i.toString
      }
      val suffix = this.unitChars.get(unit) match {
        case None => throw new UnsupportedOperationException("Don't know how to format " + unit)
        case Some(string) => string
      }
      (unit, amount + suffix)
    }
    val (dateParts, timeParts) = parts.partition(_._1.getDuration.isGreaterThan(HOURS.getDuration))
    val timeString = if (timeParts.isEmpty) "" else "T" + timeParts.map(_._2).mkString
    "P" + dateParts.map(_._2).mkString + timeString
  }

  def +(that: Period): Period = Period(
    this.mapOverUnion(that, _ + _).toMap,
    this.modifier & that.modifier)

  def -(that: Period): Period = Period(
    this.mapOverUnion(that, _ - _).toMap,
    this.modifier & that.modifier)

  def >(unit: TemporalUnit): Boolean = {
    if (this.unitAmounts.isEmpty) {
      false
    } else {
      val maxUnit = this.unitAmounts.keySet.maxBy(_.getDuration)
      maxUnit.getDuration.isGreaterThan(unit.getDuration) ||
        (maxUnit == unit && this.unitAmounts(maxUnit) > 1)
    }
  }

  def addTo(time: ZonedDateTime): ZonedDateTime = this.unitAmounts.foldLeft(time) {
    case (time, (unit, value)) => time.plus(value, unit)
  }

  def subtractFrom(time: ZonedDateTime): ZonedDateTime = this.unitAmounts.foldLeft(time) {
    case (time, (unit, value)) => time.minus(value, unit)
  }

  private def mapOverUnion(that: Period, op: (Int, Int) => Int): Iterable[(TemporalUnit, Int)] = {
    for (unit <- this.unitAmounts.keySet ++ that.unitAmounts.keySet)
      yield (unit, op(this.unitAmounts.getOrElse(unit, 0), that.unitAmounts.getOrElse(unit, 0)))
  }
}
object Period {

  final val empty = Period(Map.empty)

  final val unspecified = Period(Map(UNSPECIFIED -> Int.MaxValue))

  def fromFractional(numerator: Int, denominator: Int, unit: TemporalUnit, modifier: Modifier): Period = {
    var map = Map(unit -> (numerator / denominator))
    var currRemainder = numerator % denominator
    var currUnit = unit
    while (currRemainder != 0) {
      this.smallerUnit.get(currUnit) match {
        case None => throw new UnsupportedOperationException("Don't know how to split " + currUnit)
        case Some((multiplier, nextUnit)) =>
          val numerator = currRemainder * multiplier
          map += nextUnit -> (numerator / denominator)
          currUnit = nextUnit
          currRemainder = numerator % denominator
      }
    }
    Period(map, modifier)
  }

  private final val smallerUnit = Map[TemporalUnit, (Int, TemporalUnit)](
    YEARS -> (12, MONTHS),
    WEEKS -> (7, DAYS),
    DAYS -> (24, HOURS),
    HOURS -> (60, MINUTES),
    MINUTES -> (60, SECONDS))
}

case class PeriodSet(
    period: Period,
    modifier: Modifier = Modifier.Exact,
    quantifier: Quantifier = Quantifier.None,
    frequency: Frequency = Frequency(1)) extends Temporal {
  val timeMLValue = this.period.timeMLValue
}

case class TimeSpan(
    start: ZonedDateTime,
    end: ZonedDateTime,
    period: Period,
    modifier: Modifier) extends Temporal {

  def timeMLValueOption: Option[String] = {
    if (this.start == this.end) {
      Some(this.start.getDateTime.toString)
    } else {
      this.period.unitAmounts.toList match {
        case List((unit, 1)) if TimeSpan.truncate(this.start, unit) == this.start =>
          val fields = TimeSpan.unitToFieldsToDisplay.get(unit) match {
            case None => throw new UnsupportedOperationException("Don't know how to display " + unit)
            case Some(fields) => fields
          }
          val parts = for (field <- fields) yield {
            TimeSpan.fieldFormats.get(field) match {
              case None => throw new UnsupportedOperationException("Don't know how to format " + field)
              case Some(format) => format(this.start.get(field))
            }
          }
          Some(parts.mkString)
        case _ => None
      }
    }
  }
  
  val timeMLValue = this.timeMLValueOption.getOrElse(this.period.timeMLValue)
}
object TimeSpan {
  final val unspecifiedStart = ZonedDateTime.of(LocalDateTime.MIN, ZoneId.of("Z"))
  final val unspecifiedEnd = ZonedDateTime.of(LocalDateTime.MAX, ZoneId.of("Z"))
  
  def of(year: Int, month: Int, day: Int) = {
    val start = ZonedDateTime.of(LocalDateTime.of(year, month, day, 0, 0), ZoneId.of("Z"))
    this.startingAt(start, Period(Map(DAYS -> 1)), Modifier.Exact)
  }
  
  def of(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int) = {
    val localStart = LocalDateTime.of(year, month, day, hour, minute, second)
    val start = ZonedDateTime.of(localStart, ZoneId.of("Z"))
    this.startingAt(start, Period(Map(SECONDS -> 1)), Modifier.Exact)
  }
  
  def fromTimeMLValue(value: String) = {
    val fieldValues: Map[TemporalField, Int] = value.split("[-T:]") match {
      case Array(centuryOrDecadeOrYear) => centuryOrDecadeOrYear.length match {
        case 2 => Map(CENTURY -> centuryOrDecadeOrYear.toInt)
        case 3 => Map(DECADE -> centuryOrDecadeOrYear.toInt)
        case 4 => Map(YEAR -> centuryOrDecadeOrYear.toInt)
      }
      case Array(year, seasonOrQuarterOrMonthOrWeek) => Map(YEAR -> year.toInt) ++ {
        seasonOrQuarterOrMonthOrWeek match {
          case "SP" => Map(SPRING_OF_YEAR -> 1)
          case "SU" => Map(SUMMER_OF_YEAR -> 1)
          case "FA" => Map(FALL_OF_YEAR -> 1)
          case "WI" => Map(WINTER_OF_YEAR -> 1)
          case _ => seasonOrQuarterOrMonthOrWeek.head match {
            case 'W' => Map(ISO_WEEK.OF_YEAR -> seasonOrQuarterOrMonthOrWeek.tail.toInt)
            case 'Q' => Map(QUARTER_OF_YEAR -> seasonOrQuarterOrMonthOrWeek.tail.toInt)
            case _ => Map(MONTH_OF_YEAR -> seasonOrQuarterOrMonthOrWeek.toInt)
          }
        }
      }
      case Array(year, monthOrWeek, dayOrWeekend) => Map(YEAR -> year.toInt) ++ {
        monthOrWeek.head match {
          case 'W' => dayOrWeekend match {
            case "WE" => Map(ISO_WEEK.OF_YEAR -> monthOrWeek.tail.toInt, WEEKEND_OF_WEEK -> 1)
          }
          case _ => Map(MONTH_OF_YEAR -> monthOrWeek.toInt, DAY_OF_MONTH -> dayOrWeekend.toInt)
        }
      }
      case Array(year, month, day, hourOrPartOfDay) =>
        Map(YEAR -> year.toInt, MONTH_OF_YEAR -> month.toInt, DAY_OF_MONTH -> day.toInt) ++ {
          hourOrPartOfDay match {
            case "MO" => Map(MORNING_OF_DAY -> 1)
            case "AF" => Map(AFTERNOON_OF_DAY -> 1)
            case "EV" => Map(EVENING_OF_DAY -> 1)
            case "NI" => Map(NIGHT_OF_DAY -> 1)
            case hour => Map(HOUR_OF_DAY -> hour.toInt)
          }
        }
      case Array(year, month, day, hour, minute) =>
        Map(YEAR -> year.toInt, MONTH_OF_YEAR -> month.toInt, DAY_OF_MONTH -> day.toInt,
            HOUR_OF_DAY -> hour.toInt, MINUTE_OF_HOUR -> minute.toInt)
      case Array(year, month, day, hour, minute, second) =>
        Map(YEAR -> year.toInt, MONTH_OF_YEAR -> month.toInt, DAY_OF_MONTH -> day.toInt,
            HOUR_OF_DAY -> hour.toInt, MINUTE_OF_HOUR -> minute.toInt, SECOND_OF_MINUTE -> second.toInt)
      case _ => throw new Exception("%s %s".format(value, value.split("[-T]").toList))
    }

    // set all the requested fields
    val zero = ZonedDateTime.of(LocalDateTime.of(1, 1, 1, 0, 0), ZoneId.of("Z"))
    val nonTruncatedStart = fieldValues.foldLeft(zero) {
      case (dateTime, (field, value)) => dateTime.`with`(field, value)
    }

    // truncate the date-time based on the smallest field's base unit 
    val minField = fieldValues.keySet.minBy(_.getBaseUnit().getDuration())
    val minUnit = minField.getBaseUnit()
    val start = this.truncate(nonTruncatedStart, minUnit)

    // for things that overlap the boundary (e.g. NIGHT_OF_DAY) truncation will move them to
    // the previous range (e.g. the previous day) so we'll need to move them back
    val isNotTooEarly = fieldValues.forall { case (field, value) => start.get(field) == value }
    val adjustedStart = if (isNotTooEarly) start else start.plus(1, minField.getRangeUnit())

    // create a time span of exactly one unit in size
    this.startingAt(adjustedStart, Period(Map(minUnit -> 1)), Modifier.Exact)
  }
  
  def startingAt(start: ZonedDateTime, period: Period, modifier: Modifier): TimeSpan = {
    TimeSpan(start, period.addTo(start), period, modifier)
  }

  def endingAt(end: ZonedDateTime, period: Period, modifier: Modifier): TimeSpan = {
    TimeSpan(period.subtractFrom(end), end, period, modifier)
  }

  def truncate(time: ZonedDateTime, unit: TemporalUnit): ZonedDateTime = {
    this.unitToFieldsToTruncate.get(unit) match {
      case None => throw new UnsupportedOperationException("Don't know how to truncate " + unit)
      case Some(fields) => fields.foldLeft(time) {
        case (time, field) => {
          val nUnits = time.get(field) - field.range.getMinimum
          time.minus(nUnits, field.getBaseUnit)
        }
      }
    }
  }

  private[timenorm] val fieldFormats = Map[TemporalField, Int => String](
    (CENTURY, "%02d".format(_)),
    (DECADE, "%03d".format(_)),
    (YEAR, "%04d".format(_)),
    (QUARTER_OF_YEAR, "-Q%d".format(_)),
    (SPRING_OF_YEAR, _ match { case 1 => "-SP" }),
    (SUMMER_OF_YEAR, _ match { case 1 => "-SU" }),
    (FALL_OF_YEAR, _ match { case 1 => "-FA" }),
    (WINTER_OF_YEAR, _ match { case 1 => "-WI" }),
    (MONTH_OF_YEAR, "-%02d".format(_)),
    (DAY_OF_MONTH, "-%02d".format(_)),
    (ISO_WEEK.OF_YEAR, "-W%02d".format(_)),
    (DAY_OF_WEEK, "-%d".format(_)),
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
    QUARTER_YEARS -> Seq(YEAR, QUARTER_OF_YEAR),
    SPRINGS -> Seq(YEAR, SPRING_OF_YEAR),
    SUMMERS -> Seq(YEAR, SUMMER_OF_YEAR),
    FALLS -> Seq(YEAR, FALL_OF_YEAR),
    WINTERS -> Seq(YEAR, WINTER_OF_YEAR),
    MONTHS -> Seq(YEAR, MONTH_OF_YEAR),
    WEEKS -> Seq(YEAR, ISO_WEEK.OF_YEAR),
    WEEKENDS -> Seq(YEAR, ISO_WEEK.OF_YEAR, WEEKEND_OF_WEEK),
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
    QUARTER_YEARS -> Seq(DAY_OF_QUARTER, HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE),
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

case class TimeSpanSet(fields: Map[TemporalField, Int]) extends Temporal {
  val timeMLValue: String = {
    val (timeFields, dayFields) = fields.keySet.partition(_.getBaseUnit().getDuration().isLessThan(DAYS.getDuration()))
    val minDayField =
      if (dayFields.isEmpty) DAY_OF_MONTH
      else dayFields.minBy(_.getBaseUnit().getDuration())
    val dayFieldsToDisplay = TimeSpanSet.fieldToDayFieldsToDisplay(minDayField)
    val timeFieldsToDisplay =
      if (timeFields.isEmpty) Seq.empty[TemporalField]
      else TimeSpanSet.fieldToTimeFieldsToDisplay(timeFields.minBy(_.getBaseUnit().getDuration()))
    val fieldsToDisplay = dayFieldsToDisplay ++ timeFieldsToDisplay
    val parts =
      for (field <- fieldsToDisplay) yield fields.get(field) match {
        case Some(value) => TimeSpan.fieldFormats(field)(value)
        case None => TimeSpanSet.unspecifiedFieldFormats(field)
      }
    parts.mkString
  }
}
object TimeSpanSet {
  
  private val fieldToDayFieldsToDisplay = Map[TemporalField, Seq[TemporalField]](
    CENTURY -> Seq(CENTURY),
    DECADE -> Seq(DECADE),
    YEAR -> Seq(YEAR),
    QUARTER_OF_YEAR -> Seq(YEAR, QUARTER_OF_YEAR),
    SPRING_OF_YEAR -> Seq(YEAR, SPRING_OF_YEAR),
    SUMMER_OF_YEAR -> Seq(YEAR, SUMMER_OF_YEAR),
    FALL_OF_YEAR -> Seq(YEAR, FALL_OF_YEAR),
    WINTER_OF_YEAR -> Seq(YEAR, WINTER_OF_YEAR),
    MONTH_OF_YEAR -> Seq(YEAR, MONTH_OF_YEAR),
    ISO_WEEK.OF_YEAR -> Seq(YEAR, ISO_WEEK.OF_YEAR),
    WEEKEND_OF_WEEK -> Seq(YEAR, ISO_WEEK.OF_YEAR, WEEKEND_OF_WEEK),
    DAY_OF_WEEK -> Seq(YEAR, ISO_WEEK.OF_YEAR, DAY_OF_WEEK),
    DAY_OF_MONTH -> Seq(YEAR, MONTH_OF_YEAR, DAY_OF_MONTH))
    
  private val fieldToTimeFieldsToDisplay = Map[TemporalField, Seq[TemporalField]](
    MORNING_OF_DAY -> Seq(MORNING_OF_DAY),
    AFTERNOON_OF_DAY -> Seq(AFTERNOON_OF_DAY),
    EVENING_OF_DAY -> Seq(EVENING_OF_DAY),
    NIGHT_OF_DAY -> Seq(NIGHT_OF_DAY),
    HOUR_OF_DAY -> Seq(HOUR_OF_DAY),
    MINUTE_OF_HOUR -> Seq(HOUR_OF_DAY, MINUTE_OF_HOUR),
    SECOND_OF_MINUTE -> Seq(HOUR_OF_DAY, MINUTE_OF_HOUR, SECOND_OF_MINUTE))

  private val unspecifiedFieldFormats = Map[TemporalField, String](
    (CENTURY, "XX"),
    (DECADE, "XXX"),
    (YEAR, "XXXX"),
    (MONTH_OF_YEAR, "-XX"),
    (DAY_OF_MONTH, "-XX"),
    (ISO_WEEK.OF_YEAR, "-WXX"),
    (HOUR_OF_DAY, "TXX"),
    (MINUTE_OF_HOUR, ":XX"),
    (SECOND_OF_MINUTE, ":XX"))
}

abstract class Modifier(val timeMLValueOption: Option[String]) {
  def &(that: Modifier): Modifier = {
    if (this == that) {
      this
    } else if (this == Modifier.Exact) {
      that
    } else if (that == Modifier.Exact) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}
object Modifier {
  case object Exact extends Modifier(None)
  case object Before extends Modifier(Some("BEFORE"))
  case object After extends Modifier(Some("AFTER"))
  case object OnOrBefore extends Modifier(Some("ON_OR_BEFORE"))
  case object OnOrAfter extends Modifier(Some("ON_OR_AFTER"))
  case object LessThan extends Modifier(Some("LESS_THAN"))
  case object MoreThan extends Modifier(Some("MORE_THAN"))
  case object EqualOrLess extends Modifier(Some("EQUAL_OR_LESS"))
  case object EqualOrMore extends Modifier(Some("EQUAL_OR_MORE"))
  case object Start extends Modifier(Some("START"))
  case object Mid extends Modifier(Some("MID"))
  case object End extends Modifier(Some("END"))
  case object Approx extends Modifier(Some("APPROX"))

  val values = Seq(
    Exact,
    Before,
    After,
    OnOrBefore,
    OnOrAfter,
    LessThan,
    MoreThan,
    EqualOrLess,
    EqualOrMore,
    Start,
    Mid,
    End,
    Approx)

  private val stringToModifier =
    (for (modifier <- values; value <- modifier.timeMLValueOption) yield value -> modifier).toMap

  def valueOf(timeMLValue: String): Modifier = this.stringToModifier(timeMLValue)
}

case class Frequency(val times: Int, val unit: Option[TemporalUnit] = None) {
  def &(that: Frequency): Frequency = {
    if (this == that && this.unit.isEmpty) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}

abstract class Quantifier(val timeMLValue: Option[String]) {
  def &(that: Quantifier): Quantifier = {
    if (this == that && this.timeMLValue.isEmpty) {
      this
    } else {
      throw new IllegalArgumentException(
        "cannot combine %s and %s".format(this, that))
    }
  }
}
object Quantifier {
  case object None extends Quantifier(scala.None)
  case object Every extends Quantifier(Some("EVERY"))
  case object Each extends Quantifier(Some("EACH"))

  val values = Seq[Quantifier](None, Every, Each)

  private val stringToQuantifier =
    (for (quantifier <- values; name <- quantifier.timeMLValue) yield name -> quantifier).toMap

  def valueOf(timeMLValue: String): Quantifier = this.stringToQuantifier(timeMLValue)
}

