package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.ZonedDateTime
import scala.collection.immutable.ListMap

case class Period(unitAmounts: Map[TemporalUnit, Int], modifier: Modifier) {
  
  private val simplifyUnitMap = ListMap[TemporalUnit, (TemporalUnit, Int => Int)](
    QUARTER_DAYS -> (HOURS, _ * 6),
    DECADES -> (YEARS, _ * 10),
    CENTURIES -> (YEARS, _ * 100))

  private val unitChars = ListMap[TemporalUnit, String](
    YEARS -> "Y",
    SEASONS -> "S",
    MONTHS -> "M",
    WEEKS -> "W",
    DAYS -> "D",
    HOURS -> "H",
    MINUTES -> "M",
    SECONDS -> "S")
  
  val timeMLValue: String = {
    val simpleUnitAmounts = this.simplifyUnitMap.foldLeft(this.unitAmounts) {
      case (counts, (unit, (simpleUnit, convert))) => counts.get(unit) match {
        case None => counts
        case Some(value) =>
          val newValue = counts.getOrElse(simpleUnit, 0) + convert(value) 
          counts - unit + (simpleUnit -> newValue)
      }
    }
    val parts = for (unit <- simpleUnitAmounts.keySet.toSeq.sortBy(_.getDuration).reverse) yield {
      (unit, simpleUnitAmounts(unit) + this.unitChars(unit))
    }
    val (dateParts, timeParts) = parts.partition(_._1.getDuration.isGreaterThan(HOURS.getDuration))
    val timeString = if (timeParts.isEmpty) "" else "T" + timeParts.map(_._2).mkString
    "P" + dateParts.map(_._2).mkString + timeString
  }

  def +(that: Period): Period = {
    Period(this.mapOverUnion(that, _ + _).toMap, this.modifier & that.modifier)
  }

  def -(that: Period): Period = {
    Period(this.mapOverUnion(that, _ - _).toMap, this.modifier & that.modifier)
  }

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
  def empty = Period(Map.empty, Modifier.Exact)
}