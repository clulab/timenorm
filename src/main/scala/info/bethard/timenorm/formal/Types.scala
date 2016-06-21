package info.bethard.timenorm.formal

import java.time.temporal._
import java.time.{DateTimeException, Duration, LocalDateTime}
import java.util
import java.util.Collections.singletonList

import scala.collection.JavaConverters._

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

  val number = n match {
    case IntNumber(x) => x
    case n:Number => ???
  }

  override def addTo(temporal: Temporal): Temporal = {
    return temporal.plus( number , unit )
  }

  override def get(unit: TemporalUnit): Long = {
    if ( unit == this.unit )
      return number
    else
      throw new UnsupportedTemporalTypeException(null)
  }

  override def subtractFrom(temporal: Temporal): Temporal = {
    return temporal.minus( number, unit )
  }

  override def getUnits: java.util.List[TemporalUnit] = {
    return singletonList(unit)
  }
}

case object UnknownPeriod extends Period {
  override def addTo(temporal: Temporal): Temporal = ???

  override def get(unit: TemporalUnit): Long = ???

  override def subtractFrom(temporal: Temporal): Temporal = ???

  override def getUnits: util.List[TemporalUnit] = ???
}

case class PeriodSum(periods: Set[Period], modifier: Modifier) extends Period {

  var map = scala.collection.mutable.Map.empty[TemporalUnit,Long]

  for ( period <- periods; unit <- period.getUnits.asScala )
      map.get(unit) match {
      case Some(value) => map(unit) += period.get(unit)
      case None => map(unit) = period.get(unit)
    }

  val list = map.keys.toList.sortBy(_.getDuration()).reverse.asJava

  override def addTo(temporal: Temporal): Temporal = {
    var current = temporal

    for ((u,n) <- map)
      current = current.plus(n, u)

    current
  }

  override def get(unit: TemporalUnit): Long =
    map.getOrElse( unit, throw new UnsupportedTemporalTypeException(null))


  override def subtractFrom(temporal: Temporal): Temporal = {
    var current = temporal

    for ((u,n) <- map)
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
  val start : LocalDateTime
  val end : LocalDateTime
}

object Interval {
  def unapply(interval: Interval): Option[(LocalDateTime,LocalDateTime)] = Some(interval.start, interval.end)
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

case class SimpleInterval(start: LocalDateTime, end: LocalDateTime) extends Interval

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

/**
  * Creates an interval of a given Period length centered on a given interval. Formally:
  * This([t1,t2): Interval, Δ: Period) = [ (t1 + t2)/2 - Δ/2, (t1 + t2)/2 + Δ/2 )
  *
  * @param interval interval to center the period upon
  * @param period period of interest
  */
case class ThisPeriod(interval: Interval, period: Period) extends Interval {
  val mid = interval.start.plus(Duration.between(interval.start,interval.end).dividedBy(2))
  val halfPeriod = Duration.between(interval.start.minus(period),interval.start).dividedBy(2)
  val start = mid.minus(halfPeriod)
  val end = start.plus(period)
}

/**
  * TODO: Update formal definition
  * Finds the repeated interval(s) containing the given interval. Formally:
  * This([t1,t2): Interval, R: RepeatingInterval): RepeatingInterval
  *
  * = { [t3,t4) ∈ R: t3 ≤ t1 ∧ t2 ≤ t4 }
  * @param interval
  * @param repeatingInterval
  */
case class ThisRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Seq[Interval] {
  val rangeStart = RepeatingInterval.truncate(interval.start, repeatingInterval.range)
  val rangeEnd = RepeatingInterval.truncate(interval.end, repeatingInterval.range).plus(1,repeatingInterval.range)
  val sequence = repeatingInterval.following(rangeStart).takeWhile(!_.end.isAfter(rangeEnd)).toIndexedSeq

  override def length: Int = sequence.size

  override def iterator: Iterator[Interval] = sequence.toIterator

  override def apply(idx: Int) = sequence(idx)
}

/**
  * LastPeriod creates an interval of the given length that ends just before the given interval.
  * Formally: Last([t1,t2): Interval, Δ: Period = [t1 - Δ, t1)
  *
  * @param interval interval to shift from
  * @param period period to shift the interval by
  */
case class LastPeriod(interval: Interval, period: Period) extends Interval {
  val start = interval.start.minus(period)
  val end = interval.start
}

/**
  * LastRepeatingInterval finds the n latest repeated intervals that appear before the given interval. Formally:
  * LastRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number) =
  * n latest of {[t.start,t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class LastRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val Interval(start, end) = repeatingInterval.preceding(interval.start).next
}

/**
  * NextPeriod creates an interval of a given length that starts just after the input interval.
  * Formally: Next([t1,t2): Interval, Δ: Period = [t2, t2 + Δ)
  *
  * @param interval interval to shift from
  * @param period period to shift the interval by
  */
case class NextPeriod(interval: Interval, period: Period) extends Interval {
  val start = interval.end
  val end = interval.start.plus(period)
}

/**
  * NextRepeatingInterval finds the n earliest repeated intervals that appear after the given interval. Formally:
  * NextRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number) =
  * n earliest of {[t.start,t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class NextRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val Interval(start, end) = repeatingInterval.following(interval.end).next
}

/**
  * BeforePeriod shifts the input interval earlier by a given period length. Formally:
  * Before([t1,t2): Interval, Δ: Period) = [t1 - Δ, t2 - Δ)
  *
  * @param interval interval to shift from
  * @param period period to shift the interval by
  */
case class BeforePeriod(interval: Interval, period: Period) extends Interval {
  val start = interval.start.minus(period)
  val end = interval.end.minus(period)
}

/**
  * BeforeRepeatingInterval finds the Nth latest repeated interval before the input interval. Formally:
  * BeforeRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth latest interval {[t.start, t.end) ∈ R: t.end ≤ t1}
  *
  * @param interval interval to begin from
  * @param repeatingInterval RI that supplies the appropriate time intervals
  */
case class BeforeRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val Interval(start, end) = repeatingInterval.preceding(interval.start).next
}

/**
  * AfterPeriod shifts the input interval later by a given period length.
  * Formally: After([t1,t2): Interval, Δ: Period) = [t1 +  Δ, t2 +  Δ)
  *
  * @param interval interval to shift from
  * @param period period to shift the interval by
  */
case class AfterPeriod(interval: Interval, period: Period) extends Interval {
  val start = interval.start.plus(period)
  val end = interval.end.plus(period)
}

/**
  * AfterRepeatingInterval finds the Nth earliest repeated interval after the input interval. Formally:
  * AfterRepeatingInterval([t1,t2): Interval, R: RepeatingInterval, n: Number): Interval =
  * Nth earliest interval {[t.start, t.end) ∈ R: t2 ≤ t.start}
  *
  * @param interval
  * @param repeatingInterval
  */
case class AfterRepeatingInterval(interval: Interval, repeatingInterval: RepeatingInterval) extends Interval {
  val Interval(start, end) = repeatingInterval.following(interval.end).next
}

/**
  * This interval finds the interval between two input intervals. Formally:
  * Between([t1,t2): startInterval,[t3,t4): endInterval): Interval = [t2,t3)
  *
  * @param startInterval first interval
  * @param endInterval second interval
  */
case class Between(startInterval: Interval, endInterval: Interval) extends Interval {
  val start = startInterval.end
  val end = endInterval.start
}

/**
  * This variant of the Nth interval creates an interval that is the nth repetition of the period following the
  * start of the interval.
  * Formally: Nth([t1,t2): Interval, Δ: Period, n: N): Interval = [t1+Δ*(n-1), t1+Δ*n)
  *
  * @param interval the interval to begin from
  * @param n the number of repetitions of the period to add
  * @param period the period to scale by
  */
case class NthInterval(interval: Interval, n: Number, period: Period) extends Interval {
  val factor = n match {
    case IntNumber(x) => x.toInt
    case n:Number => ???
  }

  val start = Iterator.fill(factor-1)(period).foldLeft(interval.start)(_ plus _)
  val end = start.plus(period)
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
  val Interval(start,end)  = repeatingInterval.following(interval.start).drop(value-1).next match {
    case result if result.end.isBefore(interval.end) => result
    case _ => ???
  }
}

case class IntervalSubIntervalIntersection(interval: Interval, subInterval: RepeatingInterval) extends Interval {
  val start = ???
  val end = ???
}

trait RepeatingInterval extends TimeExpression {
  def preceding(ldt: LocalDateTime): Iterator[Interval]
  def following(ldt: LocalDateTime): Iterator[Interval]
  val base: TemporalUnit
  val range: TemporalUnit

}

private[formal] object RepeatingInterval {
  def truncate(ldt: LocalDateTime, tUnit: TemporalUnit): LocalDateTime = tUnit match {
      case ChronoUnit.CENTURIES => LocalDateTime.of(ldt.getYear/100*100,1,1,0,0)
      case ChronoUnit.DECADES => LocalDateTime.of(ldt.getYear/10*10,1,1,0,0)
      case ChronoUnit.YEARS => ldt.withDayOfYear(1).truncatedTo(ChronoUnit.DAYS)
      case ChronoUnit.MONTHS => ldt.withDayOfMonth(1).truncatedTo(ChronoUnit.DAYS)
      case ChronoUnit.WEEKS =>
        ldt.withDayOfYear(ldt.getDayOfYear-ldt.getDayOfWeek.getValue).truncatedTo(ChronoUnit.DAYS)
      case _ => ldt.truncatedTo(tUnit)
    }
}

case class UnitRepeatingInterval(unit: TemporalUnit, modifier: Modifier) extends RepeatingInterval {
  override val base = unit
  override val range = unit

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var end = RepeatingInterval.truncate( ldt, unit ).plus(1,unit)
    var start = end.minus(1, unit)

    Iterator.continually {
      end = start
      start = start.minus(1, unit)
      SimpleInterval(start, end)
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    val truncated = RepeatingInterval.truncate(ldt, unit)
    var end = if ( truncated.isBefore(ldt) ) truncated.plus(1,unit) else truncated
    var start = end.minus(1,unit)


    Iterator.continually {
      start = end
      end = start.plus(1, unit)
      SimpleInterval(start, end)
    }
  }
}

case class FieldRepeatingInterval(field: TemporalField, value: Long, modifier: Modifier) extends RepeatingInterval {
  override val base = field.getBaseUnit
  override val range = field.getRangeUnit

  override def preceding(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(ldt.`with`(field,value), field.getBaseUnit)

    if (start.isBefore(ldt))
      start = start.plus(1,field.getRangeUnit)

    Iterator.continually {
      start = start.minus(1,field.getRangeUnit)

      while (start.get(field) != value) {
        start = start.minus(1, field.getRangeUnit)

        try
          start = start.`with`(field, value)
        catch {
          case dte: DateTimeException =>
        }
      }

      SimpleInterval(start, start.plus(1,field.getBaseUnit))
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval] = {
    var start = RepeatingInterval.truncate(ldt`with`(field,value), field.getBaseUnit)

    if (start.isAfter(ldt))
      start = start.minus(1,field.getRangeUnit)

    Iterator.continually {
      start = start.plus(1, field.getRangeUnit)

      while (start.get(field) != value) {
        start = start.plus(1,field.getRangeUnit)

        try
          start = start.`with`(field,value)
        catch {
          case dte: DateTimeException =>
        }
      }

      SimpleInterval(start, start.plus(1,field.getBaseUnit))
    }
  }
}

case class NumberedRepeatingInterval(repeatingInterval: RepeatingInterval, number: Number) extends RepeatingInterval {
  override val base = ???
  override val range = ???

  override def preceding(ldt: LocalDateTime): Iterator[Interval]  = ???

  override def following(ldt: LocalDateTime): Iterator[Interval]  = ???
}

case class RepeatingIntervalUnion(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  override val base = repeatingIntervals.minBy(_.base.getDuration).base
  override val range = repeatingIntervals.maxBy(_.range.getDuration).range

  implicit val ordering: Ordering[(LocalDateTime,Duration)] =
    Ordering.Tuple2(scala.math.Ordering.fromLessThan(_ isAfter _), Ordering[Duration].reverse)

  override def preceding(ldt: LocalDateTime): Iterator[Interval]  = {
    val iterators: Set[BufferedIterator[Interval]] = repeatingIntervals.map(_.preceding(ldt).buffered)

    Iterator.continually {
      iterators.toList.sortBy {
        iterator => val interval = iterator.head
          (interval.end, Duration.between(interval.start,interval.end))
      }.head.next
    }
  }

  override def following(ldt: LocalDateTime): Iterator[Interval]  = {
    val iterators: Set[BufferedIterator[Interval]] = repeatingIntervals.map(_.following(ldt).buffered)

    Iterator.continually {
      iterators.toList.sortBy {
        iterator => val interval = iterator.head
          (interval.start, Duration.between(interval.start, interval.end))
      }.last.next
    }
  }
}

case class RepeatingIntervalIntersection(repeatingIntervals: Set[RepeatingInterval]) extends RepeatingInterval {
  override val base = repeatingIntervals.minBy(_.base.getDuration).base
  override val range = repeatingIntervals.maxBy(_.range.getDuration).range

  override def preceding(ldt: LocalDateTime): Iterator[Interval]  = ???

  override def following(ldt: LocalDateTime): Iterator[Interval]  = ???
}

case class IntervalAsRepeatingInterval(interval: Interval) extends RepeatingInterval {
  override val base = ???
  override val range = ???

  override def preceding(ldt: LocalDateTime): Iterator[Interval]  = ???

  override def following(ldt: LocalDateTime): Iterator[Interval]  = ???
}


case class TimeZone(name: String) extends TimeExpression

