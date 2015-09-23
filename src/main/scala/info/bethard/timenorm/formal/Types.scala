package info.bethard.timenorm.formal

import java.time.temporal.{TemporalField, TemporalUnit}

trait Temporal

case class Number(n: Int) extends Temporal

trait Modifier extends Temporal
object Modifier {
  case object Exact extends Modifier
}

trait Period extends Temporal
case class SimplePeriod(unit: TemporalUnit, n: Number, modifier: Modifier) extends Period
case object UnknownPeriod extends Period
case class PeriodSum(periods: Set[Period], modifier: Modifier) extends Period

trait Interval extends Temporal
case object DocumentCreationTime extends Interval
case class Year(n: Int) extends Interval
case class TwoDigitYear(interval: Interval, twoDigits: Int) extends Interval
case class LastPeriod(interval: Interval, period: Period) extends Interval
case class BeforePeriod(interval: Interval, period: Period) extends Interval

trait RepeatingInterval extends Temporal
case class UnitRepeatingInterval(unit: TemporalUnit) extends RepeatingInterval
case class FieldRepeatingInterval(field: TemporalField, value: Long) extends RepeatingInterval
case class RepeatingIntervalUnion(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval
case class RepeatingIntervalIntersection(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval
