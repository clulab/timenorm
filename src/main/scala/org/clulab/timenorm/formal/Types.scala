package org.clulab.timenorm.formal

import java.time.temporal._
import java.time.{Duration, LocalDateTime}

import org.clulab.timenorm.field.{ConstantPartialRange, MonthDayPartialRange, QUARTER_CENTURIES}

import scala.collection.JavaConverters._
import scala.util.{Success, Try}

trait TimeExpression {
  def isDefined: Boolean
}

trait Number extends TimeExpression

object Number {

  import scala.language.implicitConversions

  implicit def intToNumber(n: Int): Number = IntNumber(n)
}

case class IntNumber(n: Int) extends Number {
  val isDefined = true
}

case class FractionalNumber(number: Int, numerator: Int, denominator: Int) extends Number {
  val isDefined = true
}

case class VagueNumber(description: String) extends Number {
  val isDefined = false
}

trait Modifier extends TimeExpression {
  val isDefined = false
}

object Modifier {

  case object Exact extends Modifier

  case object Approx extends Modifier

  case object LessThan extends Modifier

  case object MoreThan extends Modifier

  case object Start extends Modifier

  case object Mid extends Modifier

  case object End extends Modifier

  case object Fiscal extends Modifier

}

/**
  * An amount of time, expressed as counts of standard time units U = {years, months, etc.}.
  * For example, a week (i.e., weeks -> 1) or three months (i.e., months -> 3). Note that periods
  * are independent of the timeline. For example, given only the period expression 10 weeks, it
  * is impossible to assign time points of the form NNNN-NN-NN NN:NN:NN to its start and end.
  */
trait Period extends TimeExpression with TemporalAmount

case class SimplePeriod(unit: TemporalUnit, n: Number, modifier: Modifier = Modifier.Exact) extends Period {

  val isDefined: Boolean = n.isDefined && modifier == Modifier.Exact

  lazy val IntNumber(number) = n

  override def addTo(temporal: Temporal): Temporal = temporal.plus(number, unit)

  override def get(unit: TemporalUnit): Long = {
    if (unit == this.unit)
      number
    else
      throw new UnsupportedTemporalTypeException(null)
  }

  override def subtractFrom(temporal: Temporal): Temporal = temporal.minus(number, unit)

  override def getUnits: java.util.List[TemporalUnit] = java.util.Collections.singletonList(unit)
}

case object UnknownPeriod extends Period {
  val isDefined = false

  override def addTo(temporal: Temporal): Temporal = throw new UnsupportedOperationException

  override def get(unit: TemporalUnit): Long = throw new UnsupportedOperationException

  override def subtractFrom(temporal: Temporal): Temporal = throw new UnsupportedOperationException

  override def getUnits: java.util.List[TemporalUnit] = throw new UnsupportedOperationException
}

case class SumP(periods: Set[Period], modifier: Modifier = Modifier.Exact) extends Period {

  val isDefined: Boolean = periods.forall(_.isDefined) && modifier == Modifier.Exact

  lazy val map: Map[TemporalUnit, Long] = Map.empty ++ periods.flatMap(_.getUnits.asScala).map {
    unit => (unit, periods.filter(_.getUnits.contains(unit)).map(_.get(unit)).sum)
  }

  lazy val list: java.util.List[TemporalUnit] = map.keys.toList.sortBy(_.getDuration()).reverse.asJava

  override def addTo(temporal: Temporal): Temporal = map.foldLeft(temporal) {
    case (current, (unit, number)) => current.plus(number, unit)
  }

  override def get(unit: TemporalUnit): Long = map.getOrElse(unit, throw new UnsupportedTemporalTypeException(null))

  override def subtractFrom(temporal: Temporal): Temporal = map.foldLeft(temporal) {
    case (current, (unit, number)) => current.minus(number, unit)
  }

  override def getUnits: java.util.List[TemporalUnit] = list
}

/**
  * An interval on the timeline, defined by a starting point using the start val (inclusive) and an ending
  * point expressed by the end val (exclusive). For example, the expression \textit{1990} corresponds to the
  * interval [1990-01-01, 1991-01-01).
  */
trait Interval extends TimeExpression {
  def start: LocalDateTime

  def end: LocalDateTime

  def contains(interval: Interval): Boolean = !interval.start.isBefore(start) && !interval.end.isAfter(end)
}

object Interval {
  def unapply(interval: Interval): Option[(LocalDateTime, LocalDateTime)] = Some(interval.start, interval.end)

  sealed trait Point extends (Interval => LocalDateTime)

  case object Start extends Point {
    override def apply(interval: Interval): LocalDateTime = interval.start
  }

  case object End extends Point {
    override def apply(interval: Interval): LocalDateTime = interval.end
  }

}

trait Intervals extends TimeExpression with Seq[Interval] {
  protected def intervals: Seq[Interval]

  override def length: Int = intervals.size

  override def iterator: Iterator[Interval] = intervals.toIterator

  override def apply(idx: Int): Interval = intervals(idx)
}

case class SimpleIntervals(intervals: Seq[Interval]) extends Intervals {
  val isDefined = true
}

case object UnknownInterval extends Interval {
  val isDefined = false

  def start: LocalDateTime = throw new UnsupportedOperationException

  def end: LocalDateTime = throw new UnsupportedOperationException
}

case class Event(description: String) extends Interval {
  val isDefined = false

  def start: LocalDateTime = throw new UnsupportedOperationException

  def end: LocalDateTime = throw new UnsupportedOperationException
}

case class SimpleInterval(start: LocalDateTime, end: LocalDateTime) extends Interval {
  val isDefined = true
}

object SimpleInterval {
  def of(year: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, 1, 1, 0, 0)
    SimpleInterval(start, start.plusYears(1))
  }

  def of(year: Int, month: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, month, 1, 0, 0)
    SimpleInterval(start, start.plusMonths(1))
  }

  def of(year: Int, month: Int, day: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, month, day, 0, 0)
    SimpleInterval(start, start.plusDays(1))
  }

  def of(year: Int, month: Int, day: Int, hour: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, month, day, hour, 0)
    SimpleInterval(start, start.plusHours(1))
  }

  def of(year: Int, month: Int, day: Int, hour: Int, minute: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, month, day, hour, minute)
    SimpleInterval(start, start.plusMinutes(1))
  }

  def of(year: Int, month: Int, day: Int, hour: Int, minute: Int, second: Int): SimpleInterval = {
    val start = LocalDateTime.of(year, month, day, hour, minute, second)
    SimpleInterval(start, start.plusSeconds(1))
  }
}

/**
  * A Year represents the interval from the first second of the year (inclusive) to the first second of the
  * next year (exclusive). The optional second parameter allows this to also represent decades (nMissingDigits=1),
  * centuries (nMissingDigits=2), etc.
  */
case class Year(digits: Int, nMissingDigits: Int = 0) extends Interval {
  val isDefined = true
  private val durationInYears = math.pow(10, nMissingDigits).toInt
  lazy val start: LocalDateTime = LocalDateTime.of(digits * durationInYears, 1, 1, 0, 0, 0, 0)
  lazy val end: LocalDateTime = start.plusYears(durationInYears)
}

/**
  * YearSuffix creates an interval by taking the year from another interval and replacing the last digits.
  * As with Year, the optional second parameter allows YearSuffix to represent decades (nMissingDigits=1),
  * centuries (nMissingDigits=2), etc.
  */
case class YearSuffix(interval: Interval, lastDigits: Int, nSuffixDigits: Int, nMissingDigits: Int = 0) extends Interval {
  val isDefined: Boolean = interval.isDefined
  val divider: Int = math.pow(10, nSuffixDigits + nMissingDigits).toInt
  val multiplier: Int = math.pow(10, nSuffixDigits).toInt
  lazy val Interval(start, end) = Year(interval.start.getYear / divider * multiplier + lastDigits, nMissingDigits)
}

private[timenorm] object PeriodUtil {
  def expand(interval: Interval, period: Period): Interval = {
    val mid = interval.start.plus(Duration.between(interval.start, interval.end).dividedBy(2))
    val halfPeriod = Duration.between(interval.start.minus(period), interval.start).dividedBy(2)
    val start = mid.minus(halfPeriod)
    SimpleInterval(start, start.plus(period))
  }

  def oneUnit(period: Period): Period = {
    SimplePeriod(period.getUnits.asScala.maxBy(_.getDuration), 1)
  }

  def expandIfLarger(interval: Interval, period: Period): Interval = {
    if (interval.end.isBefore(interval.start.plus(period))) this.expand(interval, period) else interval
  }
}

/**
  * Creates an interval of a given Period length centered on a given interval. Formally:
  * This([t1,t2): Interval, Δ: Period) = [ (t1 + t2)/2 - Δ/2, (t1 + t2)/2 + Δ/2 )
  *
  * @param interval interval to center the period upon
  * @param period   period of interest
  */
case class ThisP(interval: Interval, period: Period) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val Interval(start, end) = PeriodUtil.expand(interval, period)
}

trait This extends TimeExpression {
  val interval: Interval
  val repeatingInterval: RepeatingInterval
  lazy val intervals: Seq[Interval] = {
    // find a start that aligns to the start of the repeating interval's range unit
    val rangeStart = RepeatingInterval.truncate(interval.start, repeatingInterval.range)

    // find an end that aligns to the end of the repeating interval's range unit
    // Note that since Intervals are defined as exclusive of their end, we have to truncate from the nanosecond before
    // the end, or in some corner cases we would truncate to a time after the desired range
    val lastNano = interval.end.minus(Duration.ofNanos(1))
    val rangeEnd = RepeatingInterval.truncate(lastNano, repeatingInterval.range).plus(1, repeatingInterval.range)

    repeatingInterval.following(rangeStart).takeWhile(_.start.isBefore(rangeEnd)).toSeq
  }
  lazy val isDefined: Boolean = interval.isDefined && repeatingInterval.isDefined && intervals.length == 1
}

/**
  * Finds the repeated interval contained within the given interval. The given interval is first expanded and aligned
  * to a unit the size of the repeating interval's range. This results in the proper semantics for something like
  * "this Wednesday", which really means "the Wednesday of this week".
  *
  * @param interval          the interval identifying the boundaries of the container
  * @param repeatingInterval the repeating intervals that should be found within the container
  */
case class ThisRI(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval with This {
  lazy val Seq(Interval(start, end)) = intervals
}

/**
  * Finds the repeated interval contained within the given interval. The given interval is first expanded and aligned
  * to a unit the size of the repeating interval's range. This results in the proper semantics for something like
  * "this Wednesday", which really means "the Wednesday of this week".
  *
  * @param interval          the interval identifying the boundaries of the container
  * @param repeatingInterval the repeating intervals that should be found within the container
  */
case class ThisRIs(interval: Interval, repeatingInterval: RepeatingInterval)
  extends Intervals with This {
  // force the case class toString rather than Seq.toString
  override lazy val toString: String = scala.runtime.ScalaRunTime._toString(this)
}

/**
  * Creates an interval of the given length that ends just before the given interval.
  * Formally: Last([t1,t2): Interval, Δ: Period = [t1 - Δ, t1)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class LastP(interval: Interval, period: Period) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val start: LocalDateTime = interval.start.minus(period)
  lazy val end: LocalDateTime = interval.start
}

trait IRIN {
  val interval: Interval
  val repeatingInterval: RepeatingInterval
  val number: Number
  lazy val isDefined: Boolean = interval.isDefined && repeatingInterval.isDefined && number.isDefined
  lazy val IntNumber(integer) = number
}

trait Last extends TimeExpression with IRIN {
  val from: Interval => LocalDateTime
  lazy val intervals: Seq[Interval] = repeatingInterval.preceding(from(interval)).take(integer).toSeq
}

/**
  * Finds the latest repeated interval that appears before the given interval. Formally:
  * Last([t1,t2): Interval, R: RepeatingInterval) = latest of {[t.start,t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class LastRI(interval: Interval,
                  repeatingInterval: RepeatingInterval,
                  from: Interval.Point = Interval.Start) extends Interval with Last {
  val number = IntNumber(1)
  lazy val Seq(Interval(start, end)) = intervals
}

/**
  * Finds the n latest repeated intervals that appear before the given interval. Formally:
  * Last([t1,t2): Interval, R: RepeatingInterval, n: Number) = n latest of {[t.start,t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param number            the number of intervals ot take
  */
case class LastRIs(interval: Interval,
                   repeatingInterval: RepeatingInterval,
                   number: Number = IntNumber(1),
                   from: Interval.Point = Interval.Start)
  extends Intervals with Last {
  // force the case class toString rather than Seq.toString
  override lazy val toString: String = scala.runtime.ScalaRunTime._toString(this)
}

/**
  * Creates an interval of a given length that starts just after the input interval.
  * Formally: Next([t1,t2): Interval, Δ: Period = [t2, t2 + Δ)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class NextP(interval: Interval, period: Period) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val start: LocalDateTime = interval.end
  lazy val end: LocalDateTime = start.plus(period)
}

trait Next extends TimeExpression with IRIN {
  val from: Interval => LocalDateTime
  lazy val intervals: Seq[Interval] = repeatingInterval.following(from(interval)).take(integer).toSeq
}

/**
  * Finds the next earliest repeated intervals that appear after the given interval. Formally:
  * Next([t1,t2): Interval, R: RepeatingInterval, n: Number) = n earliest of {[t.start,t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class NextRI(interval: Interval,
                  repeatingInterval: RepeatingInterval,
                  from: Interval.Point = Interval.End) extends Interval with Next {
  val number = IntNumber(1)
  lazy val Seq(Interval(start, end)) = intervals
}

/**
  * Finds the n earliest repeated intervals that appear after the given interval. Formally:
  * Next([t1,t2): Interval, R: RepeatingInterval, n: Number) = n earliest of {[t.start,t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param number            the number of repeated intervals to take
  */
case class NextRIs(interval: Interval,
                   repeatingInterval: RepeatingInterval,
                   number: Number = IntNumber(1),
                   from: Interval.Point = Interval.End) extends Intervals with Next {
  // force the case class toString rather than Seq.toString
  override lazy val toString: String = scala.runtime.ScalaRunTime._toString(this)
}

/**
  * Shifts the input interval earlier by a given period length. Formally:
  * Before([t1,t2): Interval, Δ: Period) = [t1 - Δ - x, t2 - Δ + x)
  * where x = u1(Δ)/2 if t2 - t1 is smaller than u1(Δ) or 0 otherwise
  * where u1(Δ) is a period with the same units as Δ but only 1 unit
  *
  * In other words, the width of the resulting interval is the maximum of
  * the widths of the input interval and a 1-unit version of the period
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class BeforeP(interval: Interval, period: Period) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val Interval(start, end) = PeriodUtil.expandIfLarger(
    SimpleInterval(interval.start.minus(period), interval.end.minus(period)),
    PeriodUtil.oneUnit(period))
}

/**
  * Finds the Nth latest repeated interval before the input interval. Formally:
  * Before([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth latest interval {[t.start, t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param number            the number of intervals to skip
  */
case class BeforeRI(interval: Interval,
                    repeatingInterval: RepeatingInterval,
                    number: Number = IntNumber(1),
                    from: Interval.Point = Interval.Start)
  extends Interval with IRIN {
  lazy val Interval(start, end) = repeatingInterval.preceding(from(interval)).drop(integer - 1).next
}

/**
  * Shifts the input interval later by a given period length.
  * Formally: After([t1,t2): Interval, Δ: Period) = [t1 +  Δ - x, t2 + Δ + x)
  * where x = u1(Δ)/2 if t2 - t1 is smaller than u1(Δ) or 0 otherwise
  * where u1(Δ) is a period with the same units as Δ but only 1 unit
  *
  * In other words, the width of the resulting interval is the maximum of
  * the widths of the input interval and a 1-unit version of the period
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class AfterP(interval: Interval, period: Period) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val Interval(start, end) = PeriodUtil.expandIfLarger(
    SimpleInterval(interval.start.plus(period), interval.end.plus(period)),
    PeriodUtil.oneUnit(period))
}

/**
  * Finds the Nth earliest repeated interval after the input interval. Formally:
  * After([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth earliest interval {[t.start, t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval          interval to start from
  * @param repeatingInterval repeating intervals to search over
  */
case class AfterRI(interval: Interval,
                   repeatingInterval: RepeatingInterval,
                   number: Number = IntNumber(1),
                   from: Interval.Point = Interval.End) extends Interval with IRIN {
  lazy val Interval(start, end) = repeatingInterval.following(from(interval)).drop(integer - 1).next
}

/**
  * Finds the interval between two input intervals. Formally:
  * Between([t1,t2): startInterval,[t3,t4): endInterval): Interval = [t2,t3)
  *
  * @param startInterval first interval
  * @param endInterval   second interval
  * @param startIncluded first interval is included
  * @param endIncluded   second interval is included
  */
case class Between(startInterval: Interval, endInterval: Interval, startIncluded: Boolean = false, endIncluded: Boolean = false) extends Interval {
  val isDefined: Boolean = startInterval.isDefined && endInterval.isDefined
  lazy val (start: LocalDateTime) = startIncluded match {
    case true => startInterval.start
    case false => startInterval.end
  }
  lazy val (end: LocalDateTime) = endIncluded match {
    case true => endInterval.end
    case false => endInterval.start
  }
}

/**
  * Creates an interval that is the nth repetition of the period in of the interval. Formally:
  * Nth([t1,t2): Interval, Δ: Period, n: N, Start): Interval = [t1+Δ*(n-1), t1+Δ*n)
  * Nth([t1,t2): Interval, Δ: Period, n: N, End): Interval = [t2-Δ*(n-1), t2-Δ*n)
  *
  * @param interval the interval to begin from
  * @param index    the number of repetitions of the period to add
  * @param period   the period to scale by
  * @param from     which end of the interval to start at
  */
case class NthP(interval: Interval,
                index: Int,
                period: Period,
                from: Interval.Point = Interval.Start) extends Interval {
  val isDefined: Boolean = interval.isDefined && period.isDefined
  lazy val Interval(start, end) = {
    val periods = Iterator.fill(index - 1)(period)
    from match {
      case Interval.Start =>
        val start = periods.foldLeft(interval.start)(_ plus _)
        val end = start.plus(period)
        SimpleInterval(start, end)
      case Interval.End =>
        val end = periods.foldLeft(interval.end)(_ minus _)
        val start = end.minus(period)
        SimpleInterval(start, end)
    }
  }
}

trait IRINP extends IRIN {
  val from: Interval.Point
  lazy val intervalsFromPoint: Iterator[Interval] = from match {
    case Interval.Start => repeatingInterval.following(interval.start)
    case Interval.End => repeatingInterval.preceding(interval.end)
  }
}

/**
  * Selects the Nth subinterval of a RepeatingInterval in another Interval. Formally:
  * Nth([t1,t2): Interval, n: Number, R: RepeatingInterval, Start): Interval
  * = Nth, counting forward from t1, of {[t.start, t.end) ∈ R : t1 ≤ t.start ∧ t.end ≤ t2}
  * Nth([t1,t2): Interval, n: Number, R: RepeatingInterval, End): Interval
  * = Nth, counting backward from t2, of {[t.start, t.end) ∈ R : t1 ≤ t.start ∧ t.end ≤ t2}
  *
  *
  * @param interval          interval to start from
  * @param index             index of the group to be selected (counting from 1)
  * @param repeatingInterval repeating intervals to select from
  * @param from              which end of the interval to start at
  */
case class NthRI(interval: Interval,
                 index: Int,
                 repeatingInterval: RepeatingInterval,
                 from: Interval.Point = Interval.Start)
  extends Interval with IRINP {
  val number = IntNumber(1)
  lazy val Interval(start, end) = intervalsFromPoint.drop(index - 1).next match {
    case result if interval contains result => result
    case result => throw new UnsupportedOperationException(s"${result.end} is outside of $interval")
  }
}

/**
  * Selects the Nth group of subintervals from a RepeatingInterval in another Interval.
  *
  * @param interval          interval to start from
  * @param index             index of the group to be selected (counting from 1)
  * @param number            number of repeated intervals in each group
  * @param repeatingInterval repeating intervals to select from
  * @param from              which end of the interval to start at
  */
case class NthRIs(interval: Interval, index: Int,
                  repeatingInterval: RepeatingInterval,
                  number: Number = IntNumber(1),
                  from: Interval.Point = Interval.Start)
  extends Intervals with IRINP {
  lazy val intervals: Seq[Interval] = intervalsFromPoint.grouped(integer).drop(index - 1).next match {
    case result if result.forall(interval.contains) => result
    case result => throw new UnsupportedOperationException(s"one of $result is outside of $interval")
  }
}

case class IntersectionI(intervals: Seq[Interval]) extends Interval {
  val isDefined: Boolean = intervals.forall(_.isDefined)
  implicit val ldtOrdering: Ordering[LocalDateTime] = Ordering.fromLessThan(_ isBefore _)
  lazy val Interval(start, end) = intervals.sortBy(_.start).reduceLeft[Interval] {
    case (i1, i2) if i1.end.isAfter(i2.start) =>
      SimpleInterval(ldtOrdering.max(i1.start, i2.start), ldtOrdering.min(i1.end, i2.end))
    case _ => throw new UnsupportedOperationException("Intervals do not intersect: " + intervals)
  }
}

trait RepeatingInterval extends TimeExpression {
  def preceding(ldt: LocalDateTime): Iterator[Interval]

  def following(ldt: LocalDateTime): Iterator[Interval]

  val base: TemporalUnit
  val range: TemporalUnit
}

private[formal] object RepeatingInterval {
  def truncate(ldt: LocalDateTime, tUnit: TemporalUnit): LocalDateTime = tUnit match {
    case ChronoUnit.CENTURIES => LocalDateTime.of(ldt.getYear / 100 * 100, 1, 1, 0, 0)
    case QUARTER_CENTURIES => LocalDateTime.of(ldt.getYear / 25 * 25, 1, 1, 0, 0)
    case ChronoUnit.DECADES => LocalDateTime.of(ldt.getYear / 10 * 10, 1, 1, 0, 0)
    case ChronoUnit.YEARS => ldt.withDayOfYear(1).truncatedTo(ChronoUnit.DAYS)
    case IsoFields.QUARTER_YEARS =>
      ldt.withMonth((ldt.getMonthValue - 1) / 4 + 1).withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)
    case ChronoUnit.MONTHS => ldt.withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)
    case ChronoUnit.WEEKS => ldt.withDayOfYear((ldt.getDayOfYear - ldt.getDayOfWeek.getValue + 1) max 1).truncatedTo(ChronoUnit.DAYS)
    case range: MonthDayPartialRange => ldt.`with`(range.first).truncatedTo(ChronoUnit.DAYS)
    case range: ConstantPartialRange => ldt.`with`(range.field, range.first).truncatedTo(range.field.getBaseUnit)
    case _ => ldt.truncatedTo(tUnit)
  }
}

case class RepeatingUnit(unit: TemporalUnit, modifier: Modifier = Modifier.Exact) extends RepeatingInterval {
  val isDefined = modifier == Modifier.Exact
  override val base: TemporalUnit = unit
  override val range: TemporalUnit = unit

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var end = RepeatingInterval.truncate(ldt, unit).plus(1, unit)
    var start = end.minus(1, unit)

    Iterator.continually {
      end = start
      start = start.minus(1, unit)
      SimpleInterval(start, end)
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    val truncated = RepeatingInterval.truncate(ldt, unit)
    var end = if (truncated.isBefore(ldt)) truncated.plus(1, unit) else truncated
    var start = end.minus(1, unit)

    Iterator.continually {
      start = end
      end = start.plus(1, unit)
      SimpleInterval(start, end)
    }
  }
}

case class RepeatingField(field: TemporalField, value: Long, modifier: Modifier = Modifier.Exact) extends RepeatingInterval {
  val isDefined = field.range.isValidValue(value) && modifier == Modifier.Exact
  override val base: TemporalUnit = field.getBaseUnit
  override val range: TemporalUnit = field.getRangeUnit

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(this.withFieldValue(ldt, _.plus(_, _)), field.getBaseUnit)
    val end = start.plus(1, field.getBaseUnit)

    if (!ldt.isBefore(end)) {
      start = this.plus1range(start)
    }
    Iterator.continually {
      start = this.minus1range(start)
      SimpleInterval(start, start.plus(1, field.getBaseUnit))
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(this.withFieldValue(ldt, _.minus(_, _)), field.getBaseUnit)

    if (!start.isBefore(ldt)) {
      start = minus1range(start)
    }
    Iterator.continually {
      start = plus1range(start)
      SimpleInterval(start, start.plus(1, field.getBaseUnit))
    }
  }

  private def withFieldValue(ldt: LocalDateTime,
                             update: (LocalDateTime, Int, TemporalUnit) => LocalDateTime): LocalDateTime = {
    if (!field.range().isValidValue(value)) {
      ldt.`with`(field, value) // guaranteed to throw an exception
    }
    Iterator.from(0).map(i => Try(update(ldt, i, field.getRangeUnit).`with`(field, value))).collect{
      case Success(updatedLDT) => updatedLDT
    }.next
  }

  private def plus1range(ldt: LocalDateTime): LocalDateTime = {
    Iterator.from(1).map(i => ldt.plus(i, field.getRangeUnit)).filter(_.get(field) == value).next
  }

  private def minus1range(ldt: LocalDateTime): LocalDateTime = {
    Iterator.from(1).map(i => ldt.minus(i, field.getRangeUnit)).filter(_.get(field) == value).next
  }
}

case class UnionRI(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  val isDefined: Boolean = repeatingIntervals.forall(_.isDefined)
  override val base: TemporalUnit = repeatingIntervals.minBy(_.base.getDuration).base
  override val range: TemporalUnit = repeatingIntervals.maxBy(_.range.getDuration).range

  implicit val ordering = Ordering.Tuple2(Ordering.fromLessThan[LocalDateTime](_ isAfter _), Ordering[Duration].reverse)

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    val iterators = repeatingIntervals.map(_.preceding(ldt).buffered).toList

    Iterator.continually {
      iterators.minBy { iterator =>
        val interval = iterator.head
        (interval.end, Duration.between(interval.start, interval.end))
      }.next
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    val iterators = repeatingIntervals.map(_.following(ldt).buffered).toList

    Iterator.continually {
      iterators.maxBy { iterator =>
        val interval = iterator.head
        (interval.start, Duration.between(interval.start, interval.end))
      }.next
    }
  }
}

case class IntersectionRI(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  val isDefined: Boolean = repeatingIntervals.forall(_.isDefined)
  override val base: TemporalUnit = repeatingIntervals.minBy(_.base.getDuration).base
  override val range: TemporalUnit = repeatingIntervals.maxBy(_.range.getDuration).range

  private val sortedRepeatingIntervals = repeatingIntervals.toList
    .sortBy(ri => (ri.range.getDuration, ri.base.getDuration)).reverse

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    val startInterval = sortedRepeatingIntervals.head.preceding(ldt).next
    var startPoint = startInterval.end
    // adjustment for the case where the ldt was in the middle of the interval
    if (startInterval.start.plus(1, range).isBefore(ldt)) {
      startPoint = startPoint.plus(1, range)
    }
    val iterators = sortedRepeatingIntervals.map(_.preceding(startPoint).buffered)

    Iterator.continually {
      startPoint = startPoint.minus(1, range)
      val firstInterval = iterators.head.next
      val othersAfterStart = iterators.tail.map(it => it.takeWhile(_ => it.head.start isAfter startPoint).toList)

      othersAfterStart.iterator.foldLeft(List(firstInterval)) {
        (intersectedIntervals, newIntervals) => newIntervals.filter(overlapsWith(_, intersectedIntervals))
      }
    }.flatten
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    val startInterval = sortedRepeatingIntervals.head.following(ldt).next
    var startPoint = startInterval.start
    // adjustment for the case where the ldt was in the middle of the interval
    if (startInterval.end.minus(1, range).isAfter(ldt)) {
      startPoint = startPoint.minus(1, range)
    }
    val iterators = sortedRepeatingIntervals.map(_.following(startPoint).buffered)

    Iterator.continually {
      startPoint = startPoint.plus(1, range)
      val firstInterval = iterators.head.next
      val othersBeforeStart = iterators.tail.map(it => it.takeWhile(_ => it.head.end isBefore startPoint).toList)

      othersBeforeStart.iterator.foldLeft(List(firstInterval)) {
        (intersectedIntervals, newIntervals) => newIntervals.filter(overlapsWith(_, intersectedIntervals))
      }
    }.flatten
  }

  // check for overlap rather than containment to allow "Thursday nights", "the last week of January", etc.
  private def overlapsWith(interval: Interval, intervals: Iterable[Interval]): Boolean = {
    intervals.exists(i => (interval.start isBefore i.end) && (i.start isBefore interval.end))
  }
}

case class TimeZone(name: String) extends TimeExpression {
  val isDefined = false
}

