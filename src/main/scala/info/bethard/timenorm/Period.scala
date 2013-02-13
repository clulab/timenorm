package info.bethard.timenorm

import org.threeten.bp.temporal.TemporalUnit
import org.threeten.bp.temporal.ChronoUnit._

case class Period(unitAmounts: Map[TemporalUnit, Int], modifier: String = "EXACT") {

  private val dateChars = Seq[(TemporalUnit, String)](
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

  def +(that: Period): Period = Period(this.mapOverUnion(that, _ + _).toMap)

  def -(that: Period): Period = Period(this.mapOverUnion(that, _ - _).toMap)
  
  private def mapOverUnion(that: Period, op: (Int, Int) => Int): Iterable[(TemporalUnit, Int)] = {
    for (unit <- this.unitAmounts.keySet ++ that.unitAmounts.keySet)
      yield (unit, op(this.unitAmounts.getOrElse(unit, 0), that.unitAmounts.getOrElse(unit, 0)))
  }
}
