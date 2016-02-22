package info.bethard.timenorm.formal

import java.time.temporal.{Temporal, TemporalAmount, TemporalField, TemporalUnit}
import java.time.LocalDateTime
import java.util

trait TimeExpression

trait Number extends TimeExpression
case class IntNumber(n: Int) extends Number
case class FractionalNumber(number: Int, numerator: Int, denominator: Int) extends Number
case class VagueNumber(description: String) extends Number

trait Modifier extends TimeExpression
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

case class SimplePeriod(unit: TemporalUnit, n: Number, modifier: Modifier) extends Period {
  override def addTo(temporal: Temporal): Temporal = ???

  override def get(unit: TemporalUnit): Long = ???

  override def subtractFrom(temporal: Temporal): Temporal = ???

  override def getUnits: util.List[TemporalUnit] = ???
}

case object UnknownPeriod extends Period {
  override def addTo(temporal: Temporal): Temporal = ???

  override def get(unit: TemporalUnit): Long = ???

  override def subtractFrom(temporal: Temporal): Temporal = ???

  override def getUnits: util.List[TemporalUnit] = ???
}

case class PeriodSum(periods: Set[Period], modifier: Modifier) extends Period {
  override def addTo(temporal: Temporal): Temporal = ???

  override def get(unit: TemporalUnit): Long = ???

  override def subtractFrom(temporal: Temporal): Temporal = ???

  override def getUnits: util.List[TemporalUnit] = ???
}

/**
  * An interval on the timeline, defined by a starting point using the start val (inclusive) and an ending
  * point expressed by the end val (exclusive). For example, the expression \textit{1990} corresponds to the
  * interval [1990-01-01, 1991-01-01).
  */
trait Interval extends TimeExpression {
  val start : LocalDateTime
  val end : LocalDateTime
}

case object DocumentCreationTime extends Interval {
  val start = ???
  val end = ???
}

case object UnknownInterval extends Interval {
  val start = ???
  val end = ???
}

case class Event(description: String) extends Interval {
  val start = ???
  val end = ???
}

/**
 * A Year represents the interval from the first second of the year (inclusive) to the first second of the
 * next year (exclusive).
 */
case class Year(n: Int) extends Interval {
  val start = LocalDateTime.of( n, 1, 1, 0, 0, 0, 0 )
  val end = start.plusYears( 1 )
}

/**
 * A Decade represents the interval from the first second of the decade (inclusive) to the first second of the
 * next decade (exclusive).
 */
case class Decade(n: Int) extends Interval {
  val start = LocalDateTime.of( n * 10, 1, 1, 0, 0, 0, 0 )
  val end = start.plusYears( 10 )
}

/**
 * A Century represents an interval from the first second of the century (inclusive) to the first second
 * of the next century (exclusive).
 */
case class Century(n: Int) extends Interval {
  val start = LocalDateTime.of( n * 100, 1, 1, 0, 0, 0, 0 )
  val end = start.plusYears( 100 )
}

/**
 * TwoDigitYear creates a one year interval from two digits and the century of another interval.
 * Formally: TwoDigitYear([ABCD-EF-GH,...) : Interval, YZ : Integer) = [ABYZ-01-01, (ABYZ+1)-01-01)
 */
case class TwoDigitYear(interval: Interval, twoDigits: Int) extends Interval {
  val start = LocalDateTime.of( ( interval.start.getYear() / 100 * 100 ) + twoDigits,
      1, 1, 0, 0, 0, 0 )
  val end = start.plusYears( 1 )
}

case class ThisPeriod(interval: Interval, period: Period) extends Interval {
  val start = ???
  val end = ???
}
case class ThisRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}
case class LastPeriod(interval: Interval, period: Period) extends Interval {
  val start = ???
  val end = ???
}
case class LastRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}
case class NextPeriod(interval: Interval, period: Period) extends Interval {
  val start = ???
  val end = ???
}

case class NextRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

case class BeforePeriod(interval: Interval, period: Period) extends Interval {
  val start = ???
  val end = ???
}

case class BeforeRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

case class AfterPeriod(interval: Interval, period: Period) extends Interval {
  val start = ???
  val end = ???
}

case class AfterRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

case class Between(startInterval: Interval, endInterval: Interval) extends Interval {
  val start = ???
  val end = ???
}

case class Nth(interval: Interval, value: Int, repeatingInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

case class IntervalSubIntervalIntersection(interval: Interval, subInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

trait RepeatingInterval extends TimeExpression
case class UnitRepeatingInterval(unit: TemporalUnit, modifier: Modifier) extends RepeatingInterval
case class FieldRepeatingInterval(field: TemporalField, value: Long, modifier: Modifier) extends RepeatingInterval
case class NumberedRepeatingInterval(repeatingInterval: RepeatingInterval, number: Number) extends RepeatingInterval
case class RepeatingIntervalUnion(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval
case class RepeatingIntervalIntersection(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval
case class IntervalAsRepeatingInterval(interval: Interval) extends RepeatingInterval

case class TimeZone(name: String) extends TimeExpression
