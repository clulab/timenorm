package info.bethard.timenorm.formal

import java.time.temporal._
import java.time.{DateTimeException, Duration, LocalDateTime}
import java.util
import java.util.Collections.singletonList

import scala.collection.JavaConverters._
import scala.collection.mutable

trait TimeExpression {
  def isDefined: Boolean
}

trait Number extends TimeExpression

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

  val isDefined = n.isDefined

  def number = n match {
    case IntNumber(x) => x
    case n: Number => ???
  }

  override def addTo(temporal: Temporal): Temporal = {
    return temporal.plus(number, unit)
  }

  override def get(unit: TemporalUnit): Long = {
    if (unit == this.unit)
      return number
    else
      throw new UnsupportedTemporalTypeException(null)
  }

  override def subtractFrom(temporal: Temporal): Temporal = {
    return temporal.minus(number, unit)
  }

  override def getUnits: java.util.List[TemporalUnit] = {
    return singletonList(unit)
  }
}

case object UnknownPeriod extends Period {

  val isDefined = false

  override def addTo(temporal: Temporal): Temporal = ???

  override def get(unit: TemporalUnit): Long = ???

  override def subtractFrom(temporal: Temporal): Temporal = ???

  override def getUnits: util.List[TemporalUnit] = ???
}

case class PeriodSum(periods: Set[Period], modifier: Modifier = Modifier.Exact) extends Period {

  val isDefined = periods.forall(_.isDefined)

  lazy val map = {
    val map = scala.collection.mutable.Map.empty[TemporalUnit, Long]
    for (period <- periods; unit <- period.getUnits.asScala)
      map.get(unit) match {
        case Some(value) => map(unit) += period.get(unit)
        case None => map(unit) = period.get(unit)
      }
    map
  }

  lazy val list = map.keys.toList.sortBy(_.getDuration()).reverse.asJava

  override def addTo(temporal: Temporal): Temporal = {
    var current = temporal

    for ((u, n) <- map)
      current = current.plus(n, u)

    current
  }

  override def get(unit: TemporalUnit): Long =
    map.getOrElse(unit, throw new UnsupportedTemporalTypeException(null))


  override def subtractFrom(temporal: Temporal): Temporal = {
    var current = temporal

    for ((u, n) <- map)
      current = current.minus(n, u)

    current
  }

  override def getUnits: util.List[TemporalUnit] = list
}

/**
  * An interval on the timeline, defined by a starting point using the start val (inclusive) and an ending
  * point expressed by the end val (exclusive). For example, the expression \textit{1990} corresponds to the
  * interval [1990-01-01, 1991-01-01).
  */
trait Interval extends TimeExpression {
  def start: LocalDateTime

  def end: LocalDateTime
}

object Interval {
  def unapply(interval: Interval): Option[(LocalDateTime, LocalDateTime)] = Some(interval.start, interval.end)
}

trait Intervals extends TimeExpression with Seq[Interval] {
  protected def intervals: Seq[Interval]

  override def length: Int = intervals.size

  override def iterator: Iterator[Interval] = intervals.toIterator

  override def apply(idx: Int) = intervals(idx)
}

case object DocumentCreationTime extends Interval {
  val isDefined = false

  def start = ???

  def end = ???
}

case object UnknownInterval extends Interval {
  val isDefined = false

  def start = ???

  def end = ???
}

case class Event(description: String) extends Interval {
  val isDefined = false

  def start = ???

  def end = ???
}

case class SimpleInterval(start: LocalDateTime, end: LocalDateTime) extends Interval {
  val isDefined = true
}

/**
  * A Year represents the interval from the first second of the year (inclusive) to the first second of the
  * next year (exclusive).
  */
case class Year(n: Int) extends Interval {
  val isDefined = true
  lazy val start = LocalDateTime.of(n, 1, 1, 0, 0, 0, 0)
  lazy val end = start.plusYears(1)
}

/**
  * A Decade represents the interval from the first second of the decade (inclusive) to the first second of the
  * next decade (exclusive).
  */
case class Decade(n: Int) extends Interval {
  val isDefined = true
  lazy val start = LocalDateTime.of(n * 10, 1, 1, 0, 0, 0, 0)
  lazy val end = start.plusYears(10)
}

/**
  * A Century represents an interval from the first second of the century (inclusive) to the first second
  * of the next century (exclusive).
  */
case class Century(n: Int) extends Interval {
  val isDefined = true
  lazy val start = LocalDateTime.of(n * 100, 1, 1, 0, 0, 0, 0)
  lazy val end = start.plusYears(100)
}

/**
  * TwoDigitYear creates a one year interval from two digits and the century of another interval.
  * Formally: TwoDigitYear([ABCD-EF-GH,...) : Interval, YZ : Integer) = [ABYZ-01-01, (ABYZ+1)-01-01)
  */
case class TwoDigitYear(interval: Interval, twoDigits: Int) extends Interval {
  val isDefined = interval.isDefined
  lazy val Interval(start, end) = Year(interval.start.getYear() / 100 * 100 + twoDigits)
}

/**
  * Creates an interval of a given Period length centered on a given interval. Formally:
  * This([t1,t2): Interval, Δ: Period) = [ (t1 + t2)/2 - Δ/2, (t1 + t2)/2 + Δ/2 )
  *
  * @param interval interval to center the period upon
  * @param period   period of interest
  */
case class ThisPeriod(interval: Interval, period: Period) extends Interval {
  val isDefined = interval.isDefined && period.isDefined
  lazy val start = {
    val mid = interval.start.plus(Duration.between(interval.start, interval.end).dividedBy(2))
    val halfPeriod = Duration.between(interval.start.minus(period), interval.start).dividedBy(2)
    mid.minus(halfPeriod)
  }
  lazy val end = start.plus(period)
}

trait This extends TimeExpression {
  protected def getIntervals(interval: Interval, repeatingInterval: RepeatingInterval) = {
    // find a start that aligns to the start of the repeating interval's range unit
    val rangeStart = RepeatingInterval.truncate(interval.start, repeatingInterval.range)

    // find an end that aligns to the end of the repeating interval's range unit
    // Note that since Intervals are defined as exclusive of their end, we have to truncate from the nanosecond before
    // the end, or in some corner cases we would truncate to a time after the desired range
    val lastNano = interval.end.minus(Duration.ofNanos(1))
    val rangeEnd = RepeatingInterval.truncate(lastNano, repeatingInterval.range).plus(1, repeatingInterval.range)

    repeatingInterval.following(rangeStart).takeWhile(!_.end.isAfter(rangeEnd)).toSeq
  }
}

/**
  * Finds the repeated interval contained within the given interval. The given interval is first expanded and aligned
  * to a unit the size of the repeating interval's range. This results in the proper semantics for something like
  * "this Wednesday", which really means "the Wednesday of this week".
  *
  * @param interval          the interval identifying the boundaries of the container
  * @param repeatingInterval the repeating intervals that should be found within the container
  */
case class ThisRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval with This {
  val isDefined = interval.isDefined && repeatingInterval.isDefined
  lazy val Seq(Interval(start, end)) = getIntervals(interval, repeatingInterval)
}

/**
  * Finds the repeated interval contained within the given interval. The given interval is first expanded and aligned
  * to a unit the size of the repeating interval's range. This results in the proper semantics for something like
  * "this Wednesday", which really means "the Wednesday of this week".
  *
  * @param interval          the interval identifying the boundaries of the container
  * @param repeatingInterval the repeating intervals that should be found within the container
  */
case class ThisRepeatingIntervals(interval: Interval, repeatingInterval: RepeatingInterval)
  extends Intervals with This {
  val isDefined = interval.isDefined && repeatingInterval.isDefined
  lazy val intervals = getIntervals(interval, repeatingInterval)
}

/**
  * LastPeriod creates an interval of the given length that ends just before the given interval.
  * Formally: Last([t1,t2): Interval, Δ: Period = [t1 - Δ, t1)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class LastPeriod(interval: Interval, period: Period) extends Interval {
  val isDefined = interval.isDefined && period.isDefined
  lazy val start = interval.start.minus(period)
  lazy val end = interval.start
}

trait Last extends TimeExpression {
  protected def getIntervals(interval: Interval, repeatingInterval: RepeatingInterval, n: Number) = n match {
    case IntNumber(number) => repeatingInterval.preceding(interval.start).take(number).toSeq
    case _ => ???
  }
}

/**
  * LastRepeatingInterval finds the latest repeated interval that appears before the given interval. Formally:
  * LastRepeatingInterval([t1,t2): Interval, R: RepeatingInterval) =
  * latest of {[t.start,t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class LastRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval with Last {
  val isDefined = interval.isDefined && repeatingInterval.isDefined
  lazy val Seq(Interval(start, end)) = getIntervals(interval, repeatingInterval, IntNumber(1))
}

/**
  * LastRepeatingIntervals finds the n latest repeated intervals that appear before the given interval. Formally:
  * LastRepeatingIntervals([t1,t2): Interval, R: RepeatingInterval, n: Number) =
  * n latest of {[t.start,t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param n                 the number of intervals ot take
  */
case class LastRepeatingIntervals(interval: Interval,
                                  repeatingInterval: RepeatingInterval,
                                  n: Number = IntNumber(1)) extends Intervals with Last {
  val isDefined = interval.isDefined && repeatingInterval.isDefined && n.isDefined
  lazy val intervals = getIntervals(interval, repeatingInterval, n)
}

/**
  * NextPeriod creates an interval of a given length that starts just after the input interval.
  * Formally: Next([t1,t2): Interval, Δ: Period = [t2, t2 + Δ)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class NextPeriod(interval: Interval, period: Period) extends Interval {
  val isDefined = interval.isDefined && period.isDefined
  lazy val start = interval.end
  lazy val end = interval.start.plus(period)
}

trait Next extends TimeExpression {
  protected def getIntervals(interval: Interval,
                             repeatingInterval: RepeatingInterval,
                             n: Number = IntNumber(1)) = n match {
    case IntNumber(number) => repeatingInterval.following(interval.end).take(number).toSeq
    case _ => ???
  }
}

/**
  * NextRepeatingInterval finds the next earliest repeated intervals that appear after the given interval. Formally:
  * NextRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number) =
  * n earliest of {[t.start,t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class NextRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval with Next {
  val isDefined = interval.isDefined && repeatingInterval.isDefined
  lazy val Seq(Interval(start, end)) = getIntervals(interval, repeatingInterval, IntNumber(1))
}

/**
  * NextRepeatingInterval finds the n earliest repeated intervals that appear after the given interval. Formally:
  * NextRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number) =
  * n earliest of {[t.start,t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param n                 the number of repeated intervals to take
  */
case class NextRepeatingIntervals(interval: Interval, repeatingInterval: RepeatingInterval, n: Number = IntNumber(1))
  extends Intervals with Next {
  val isDefined = interval.isDefined && repeatingInterval.isDefined && n.isDefined
  lazy val intervals = getIntervals(interval, repeatingInterval, n)
}

/**
  * BeforePeriod shifts the input interval earlier by a given period length. Formally:
  * Before([t1,t2): Interval, Δ: Period) = [t1 - Δ, t2 - Δ)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class BeforePeriod(interval: Interval, period: Period) extends Interval {
  val isDefined = interval.isDefined && period.isDefined
  lazy val start = interval.start.minus(period)
  lazy val end = interval.end.minus(period)
}

/**
  * BeforeRepeatingInterval finds the Nth latest repeated interval before the input interval. Formally:
  * BeforeRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth latest interval {[t.start, t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval          interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  * @param n                 the number of intervals to skip
  */
case class BeforeRepeatingInterval(interval: Interval,
                                   repeatingInterval: RepeatingInterval,
                                   n: Number = IntNumber(1)) extends Interval {
  val isDefined = interval.isDefined && repeatingInterval.isDefined && n.isDefined
  lazy val Interval(start, end) = n match {
    case IntNumber(number) => repeatingInterval.preceding(interval.start).drop(number - 1).next
    case _ => ???
  }
}

/**
  * AfterPeriod shifts the input interval later by a given period length.
  * Formally: After([t1,t2): Interval, Δ: Period) = [t1 +  Δ, t2 +  Δ)
  *
  * @param interval interval to shift from
  * @param period   period to shift the interval by
  */
case class AfterPeriod(interval: Interval, period: Period) extends Interval {
  val isDefined = interval.isDefined && period.isDefined
  lazy val start = interval.start.plus(period)
  lazy val end = interval.end.plus(period)
}

/**
  * AfterRepeatingInterval finds the Nth earliest repeated interval after the input interval. Formally:
  * AfterRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth earliest interval {[t.start, t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval
  * @param repeatingInterval
  */
case class AfterRepeatingInterval(interval: Interval,
                                  repeatingInterval: RepeatingInterval,
                                  n: Number = IntNumber(1)) extends Interval {
  val isDefined = interval.isDefined && repeatingInterval.isDefined && n.isDefined
  lazy val Interval(start, end) = n match {
    case IntNumber(number) => repeatingInterval.following(interval.end).drop(number - 1).next
    case _ => ???
  }
}

/**
  * This interval finds the interval between two input intervals. Formally:
  * Between([t1,t2): startInterval,[t3,t4): endInterval): Interval = [t2,t3)
  *
  * @param startInterval first interval
  * @param endInterval   second interval
  */
case class Between(startInterval: Interval, endInterval: Interval) extends Interval {
  val isDefined = startInterval.isDefined && endInterval.isDefined
  lazy val start = startInterval.end
  lazy val end = endInterval.start
}

/**
  * This variant of the Nth interval creates an interval that is the nth repetition of the period following the
  * start of the interval.
  * Formally: Nth([t1,t2): Interval, Δ: Period, n: N): Interval = [t1+Δ*(n-1), t1+Δ*n)
  *
  * @param interval the interval to begin from
  * @param n        the number of repetitions of the period to add
  * @param period   the period to scale by
  */
case class NthInterval(interval: Interval, n: Number, period: Period) extends Interval {
  val isDefined = interval.isDefined && n.isDefined && period.isDefined
  lazy val start = n match {
    case IntNumber(number) => Iterator.fill(number - 1)(period).foldLeft(interval.start)(_ plus _)
    case _ => ???
  }
  lazy val end = start.plus(period)
}

/**
  * NthRepeatingInterval selects the Nth subinterval of a RepeatingInterval, counting from the start
  * of another Interval. Formally: NthRepeatingInterval([t1,t2): Interval, n: Number,
  * R: RepeatingInterval): Interval = Nth of {[t.start, t.end) ∈ R : t1 ≤ t.start ∧ t.end ≤ t2}
  *
  * @param interval
  * @param value
  * @param repeatingInterval
  */
case class Nth(interval: Interval, value: Int, repeatingInterval: RepeatingInterval) extends Interval {
  val isDefined = interval.isDefined && repeatingInterval.isDefined
  lazy val Interval(start, end) = repeatingInterval.following(interval.start).drop(value - 1).next match {
    case result if result.end.isBefore(interval.end) => result
    case _ => ???
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
    case ChronoUnit.DECADES => LocalDateTime.of(ldt.getYear / 10 * 10, 1, 1, 0, 0)
    case ChronoUnit.YEARS => ldt.withDayOfYear(1).truncatedTo(ChronoUnit.DAYS)
    case ChronoUnit.MONTHS => ldt.withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)
    case ChronoUnit.WEEKS =>
      ldt.withDayOfYear(ldt.getDayOfYear - ldt.getDayOfWeek.getValue).truncatedTo(ChronoUnit.DAYS)
    case _ => ldt.truncatedTo(tUnit)
  }
}

case class UnitRepeatingInterval(unit: TemporalUnit, modifier: Modifier = Modifier.Exact) extends RepeatingInterval {
  val isDefined = true
  override val base = unit
  override val range = unit

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

case class FieldRepeatingInterval(field: TemporalField, value: Long, modifier: Modifier = Modifier.Exact) extends RepeatingInterval {
  val isDefined = true
  override val base = field.getBaseUnit
  override val range = field.getRangeUnit

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(ldt.`with`(field, value), field.getBaseUnit)

    if (!start.isAfter(ldt))
      start = start.plus(1, field.getRangeUnit)

    Iterator.continually {
      start = start.minus(1, field.getRangeUnit)

      while (start.get(field) != value) {
        start = start.minus(1, field.getRangeUnit)

        try
          start = start.`with`(field, value)
        catch {
          case dte: DateTimeException =>
        }
      }

      SimpleInterval(start, start.plus(1, field.getBaseUnit))
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(ldt `with`(field, value), field.getBaseUnit)

    if (!start.isBefore(ldt))
      start = start.minus(1, field.getRangeUnit)

    Iterator.continually {
      start = start.plus(1, field.getRangeUnit)

      while (start.get(field) != value) {
        start = start.plus(1, field.getRangeUnit)

        try
          start = start.`with`(field, value)
        catch {
          case dte: DateTimeException =>
        }
      }

      SimpleInterval(start, start.plus(1, field.getBaseUnit))
    }
  }
}

case class RepeatingIntervalUnion(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  val isDefined = repeatingIntervals.forall(_.isDefined)
  override val base = repeatingIntervals.minBy(_.base.getDuration).base
  override val range = repeatingIntervals.maxBy(_.range.getDuration).range

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

case class RepeatingIntervalIntersection(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  val isDefined = repeatingIntervals.forall(_.isDefined)
  override val base = repeatingIntervals.minBy(_.base.getDuration).base
  override val range = repeatingIntervals.maxBy(_.range.getDuration).range

  private val sortedRepeatingIntervals = repeatingIntervals.toList
    .sortBy(ri => (ri.range.getDuration, ri.base.getDuration)).reverse

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var startPoint = sortedRepeatingIntervals.head.preceding(ldt).next.end
    val iterators = sortedRepeatingIntervals.map(_.preceding(startPoint).buffered)

    Iterator.continually {
      startPoint = startPoint.minus(1, range)
      val firstInterval = iterators.head.next
      val othersAfterStart = iterators.tail.map(it => it.takeWhile(_ => it.head.start isAfter startPoint).toList)

      othersAfterStart.iterator.foldLeft(List(firstInterval)) {
        (intersectedIntervals, newIntervals) => newIntervals.filter(isContainedInOneOf(_, intersectedIntervals))
      }
    }.flatten
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    var startPoint = sortedRepeatingIntervals.head.following(ldt).next.start
    val iterators = sortedRepeatingIntervals.map(_.following(startPoint).buffered)

    Iterator.continually {
      startPoint = startPoint.plus(1, range)
      val firstInterval = iterators.head.next
      val othersBeforeStart = iterators.tail.map(it => it.takeWhile(_ => it.head.end isBefore startPoint).toList)

      othersBeforeStart.iterator.foldLeft(List(firstInterval)) {
        (intersectedIntervals, newIntervals) => newIntervals.filter(isContainedInOneOf(_, intersectedIntervals))
      }
    }.flatten
  }

  private def isContainedInOneOf(interval: Interval, intervals: Iterable[Interval]): Boolean = {
    intervals.exists(i => !(interval.start isBefore i.start) && !(interval.end isAfter i.end))
  }
}

case class TimeZone(name: String) extends TimeExpression {
  val isDefined = false
}

