package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.ZonedDateTime
import scala.collection.immutable.ListMap

case class Period(
    unitAmounts: Map[TemporalUnit, Int],
    modifier: Modifier = Modifier.Exact,
    quantifier: Quantifier = Quantifier.None,
    frequency: Frequency = Frequency(1)) {
  
  private val simplifyUnitMap = ListMap[TemporalUnit, Seq[(TemporalUnit, Int)]](
    DECADES -> Seq((YEARS, 10)),
    CENTURIES -> Seq((DECADES, 10), (YEARS, 100)))

  private val unitChars = ListMap[TemporalUnit, String](
    FOREVER -> "",
    CENTURIES -> "CE",
    DECADES -> "DE",
    YEARS -> "Y",
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
      (unit, amount + this.unitChars(unit))
    }
    val (dateParts, timeParts) = parts.partition(_._1.getDuration.isGreaterThan(HOURS.getDuration))
    val timeString = if (timeParts.isEmpty) "" else "T" + timeParts.map(_._2).mkString
    "P" + dateParts.map(_._2).mkString + timeString
  }

  def +(that: Period): Period = Period(
    this.mapOverUnion(that, _ + _).toMap,
    this.modifier & that.modifier,
    this.quantifier & that.quantifier,
    this.frequency & that.frequency)

  def -(that: Period): Period = Period(
    this.mapOverUnion(that, _ - _).toMap,
    this.modifier & that.modifier,
    this.quantifier & that.quantifier,
    this.frequency & that.frequency)

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
  def empty = Period(Map.empty)
  def infinite = Period(Map(FOREVER -> Int.MaxValue))
  def fromFractional(numerator: Int, denominator: Int, unit: TemporalUnit, modifier: Modifier): Period = {
    var map = Map(unit -> (numerator / denominator))
    var currRemainder = numerator % denominator
    var currUnit = unit
    while (currRemainder != 0) {
      val (multiplier, nextUnit) = this.smallerUnit(currUnit)
      val numerator = currRemainder * multiplier
      map += nextUnit -> (numerator / denominator)
      currUnit = nextUnit
      currRemainder = numerator % denominator
    }
    Period(map, modifier)
  }
  
  private final val smallerUnit = Map[TemporalUnit, (Int, TemporalUnit)](
      WEEKS -> (7, DAYS),
      DAYS -> (24, HOURS),
      HOURS -> (60, MINUTES),
      MINUTES -> (60, SECONDS))
}