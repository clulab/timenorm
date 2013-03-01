package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoUnit._
import org.threeten.bp.ZonedDateTime

case class Period(unitAmounts: Map[TemporalUnit, Int], modifier: Modifier) {

  private val dateChars = Seq[(TemporalUnit, String)](
    DECADES -> "0Y",
    YEARS -> "Y",
    MONTHS -> "M",
    WEEKS -> "W",
    DAYS -> "D")

  private val timeChars = Seq[(TemporalUnit, String)](
    HOURS -> "H",
    MINUTES -> "M",
    SECONDS -> "S")

  private def toParts(counts: Map[TemporalUnit, Int], unitChars: Seq[(TemporalUnit, String)]) = {
    val partOptions = for ((unit, char) <- unitChars) yield counts.get(unit).map(_ + char)
    partOptions.flatten
  }

  val timeMLValue: String = {
    val dateParts = this.toParts(this.unitAmounts, this.dateChars)
    val timeParts = this.toParts(this.unitAmounts, this.timeChars)
    val timeString = if (timeParts.isEmpty) "" else "T" + timeParts.mkString
    "P" + dateParts.mkString + timeString
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